//! Helpers for include/exclude lists.
use bitflags::bitflags;

use crate::PatternFlag;

#[rustfmt::skip]
bitflags! {
    /// These flags influence what kind of paths should be matched.
    pub struct MatchFlag: u16 {
        /// Match only a complete entry. The pattern `bar` will not match `/foo/bar`.
        const ANCHORED            = 0x00_01;

        const MATCH_DIRECTORIES   = 0x01_00;
        const MATCH_REGULAR_FILES = 0x02_00;
        const MATCH_SYMLINKS      = 0x04_00;
        const MATCH_SOCKETS       = 0x08_00;
        const MATCH_FIFOS         = 0x10_00;
        const MATCH_CHARDEVS      = 0x20_00;
        const MATCH_BLOCKDEVS     = 0x40_00;
        const MATCH_DEVICES =
            MatchFlag::MATCH_CHARDEVS.bits() | MatchFlag::MATCH_BLOCKDEVS.bits();

        /// This is the default.
        const ANY_FILE_TYPE =
              MatchFlag::MATCH_DIRECTORIES.bits()
            | MatchFlag::MATCH_REGULAR_FILES.bits()
            | MatchFlag::MATCH_SYMLINKS.bits()
            | MatchFlag::MATCH_SOCKETS.bits()
            | MatchFlag::MATCH_FIFOS.bits()
            | MatchFlag::MATCH_CHARDEVS.bits()
            | MatchFlag::MATCH_BLOCKDEVS.bits();
    }
}

impl Default for MatchFlag {
    fn default() -> Self {
        Self::ANY_FILE_TYPE
    }
}

#[derive(Debug, PartialEq)]
pub struct FileModeRequired;

impl std::fmt::Display for FileModeRequired {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("file mode is required for matching")
    }
}

impl std::error::Error for FileModeRequired {}

pub trait GetFileMode {
    type Error;
    fn get(self) -> Result<u32, Self::Error>;
}

impl<T, E> GetFileMode for T
where
    T: FnOnce() -> Result<u32, E>,
{
    type Error = E;
    fn get(self) -> Result<u32, Self::Error> {
        self()
    }
}

impl GetFileMode for Option<u32> {
    type Error = FileModeRequired;
    fn get(self) -> Result<u32, Self::Error> {
        self.ok_or(FileModeRequired)
    }
}

impl GetFileMode for u32 {
    type Error = std::convert::Infallible;
    fn get(self) -> Result<u32, Self::Error> {
        Ok(self)
    }
}

/// A pattern entry. (Glob patterns or literal patterns.)
// Note:
// For regex we'd likely use the POSIX extended REs via `regexec(3)`, since we're targetting
// command line interfaces and want something command line users are used to.
#[derive(Clone, Debug)]
pub enum MatchPattern {
    /// A glob pattern.
    Pattern(crate::Pattern),

    /// A literal match.
    Literal(Vec<u8>),
}

impl From<crate::Pattern> for MatchPattern {
    fn from(pattern: crate::Pattern) -> Self {
        MatchPattern::Pattern(pattern)
    }
}

impl MatchPattern {
    pub fn literal(literal: impl Into<Vec<u8>>) -> Self {
        MatchPattern::Literal(literal.into())
    }
}

/// A pattern can be used as an include or an exclude pattern. In a [`MatchEntry`] list, later
/// patterns take precedence over earlier patterns and the order of includes vs excludes makes a
/// difference.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MatchType {
    Include,
    Exclude,
}

/// Convenience helpers
impl MatchType {
    pub fn is_include(self) -> bool {
        self == MatchType::Include
    }

    pub fn is_exclude(self) -> bool {
        self == MatchType::Exclude
    }
}

impl std::ops::Not for MatchType {
    type Output = MatchType;

    fn not(self) -> Self::Output {
        match self {
            MatchType::Include => MatchType::Exclude,
            MatchType::Exclude => MatchType::Include,
        }
    }
}

/// A single entry in a [`MatchList`].
#[derive(Clone, Debug)]
pub struct MatchEntry {
    pattern: MatchPattern,
    ty: MatchType,
    flags: MatchFlag,
}

impl MatchEntry {
    /// Create a new match entry.
    pub fn new<T: Into<MatchPattern>>(pattern: T, ty: MatchType) -> Self {
        Self {
            pattern: pattern.into(),
            ty,
            flags: MatchFlag::default(),
        }
    }

    /// Create a new include-type match entry with default flags.
    pub fn include<T: Into<MatchPattern>>(pattern: T) -> Self {
        Self::new(pattern.into(), MatchType::Include)
    }

    /// Create a new exclude-type match entry with default flags.
    pub fn exclude<T: Into<MatchPattern>>(pattern: T) -> Self {
        Self::new(pattern.into(), MatchType::Exclude)
    }

    /// Builder method to set the match flags to a specific value.
    pub fn flags(mut self, flags: MatchFlag) -> Self {
        self.flags = flags;
        self
    }

    /// Builder method to add flag bits to the already present ones.
    pub fn add_flags(mut self, flags: MatchFlag) -> Self {
        self.flags.insert(flags);
        self
    }

    /// Builder method to remove match flag bits.
    pub fn remove_flags(mut self, flags: MatchFlag) -> Self {
        self.flags.remove(flags);
        self
    }

    /// Builder method to toggle flag bits.
    pub fn toggle_flags(mut self, flags: MatchFlag) -> Self {
        self.flags.toggle(flags);
        self
    }

    #[inline]
    pub fn match_type(&self) -> MatchType {
        self.ty
    }

    /// Non-Builder method to change the match type.
    pub fn match_type_mut(&mut self) -> &mut MatchType {
        &mut self.ty
    }

    /// Directly access the pattern.
    pub fn pattern(&self) -> &MatchPattern {
        &self.pattern
    }

    /// Non-Builder method to change the pattern.
    pub fn pattern_mut(&mut self) -> &mut MatchPattern {
        &mut self.pattern
    }

    /// Directly access the match flags.
    pub fn match_flags(&self) -> MatchFlag {
        self.flags
    }

    /// Non-Builder method to change the flags.
    pub fn match_flags_mut(&mut self) -> &mut MatchFlag {
        &mut self.flags
    }

    /// Parse a pattern into a [`MatchEntry`] while interpreting a leading exclamation mark as
    /// inversion and trailing slashes to match only directories.
    pub fn parse_pattern<T: AsRef<[u8]>>(
        pattern: T,
        pattern_flags: PatternFlag,
        ty: MatchType,
    ) -> Result<Self, crate::ParseError> {
        Self::parse_pattern_do(pattern.as_ref(), pattern_flags, ty)
    }

    fn parse_pattern_do(
        pattern: &[u8],
        pattern_flags: PatternFlag,
        ty: MatchType,
    ) -> Result<Self, crate::ParseError> {
        let (pattern, ty) = if pattern.first().copied() == Some(b'!') {
            (&pattern[1..], !ty)
        } else {
            (pattern, ty)
        };

        let (pattern, flags) = match pattern.iter().rposition(|&b| b != b'/') {
            Some(pos) if (pos + 1) == pattern.len() => (pattern, MatchFlag::default()),
            Some(pos) => (&pattern[..=pos], MatchFlag::MATCH_DIRECTORIES),
            None => (b"/".as_ref(), MatchFlag::MATCH_DIRECTORIES),
        };

        Ok(Self::new(crate::Pattern::new(pattern, pattern_flags)?, ty).flags(flags))
    }

    /// Test this entry's file type restrictions against a file mode retrieved from `stat()`.
    pub fn matches_mode(&self, file_mode: u32) -> bool {
        // bitflags' `.contains` means ALL bits must be set, if they are all set we don't
        // need to check the mode...
        if !self.needs_file_mode() {
            return true;
        }

        let flag = match file_mode & libc::S_IFMT {
            libc::S_IFDIR => MatchFlag::MATCH_DIRECTORIES,
            libc::S_IFREG => MatchFlag::MATCH_REGULAR_FILES,
            libc::S_IFLNK => MatchFlag::MATCH_SYMLINKS,
            libc::S_IFSOCK => MatchFlag::MATCH_SOCKETS,
            libc::S_IFIFO => MatchFlag::MATCH_FIFOS,
            libc::S_IFCHR => MatchFlag::MATCH_CHARDEVS,
            libc::S_IFBLK => MatchFlag::MATCH_BLOCKDEVS,
            _unknown => return false,
        };
        self.flags.intersects(flag)
    }

    /// Test whether this entry's pattern matches any complete suffix of a path.
    ///
    /// For the path `/foo/bar/baz`, this tests whether `baz`, `bar/baz` or `foo/bar/baz` is
    /// matched.
    pub fn matches_path_suffix<T: AsRef<[u8]>>(&self, path: T) -> bool {
        self.matches_path_suffix_do(path.as_ref())
    }

    fn matches_path_suffix_do(&self, path: &[u8]) -> bool {
        if self.flags.intersects(MatchFlag::ANCHORED) {
            return self.matches_path_exact(path);
        }

        if path.is_empty() {
            return false;
        }

        for start in (0..path.len()).rev() {
            if path[start] == b'/' && self.matches_path_exact(&path[(start + 1)..]) {
                return true;
            }
        }

        // and try the whole string as well:
        self.matches_path_exact(path)
    }

    /// Test whether this entry's pattern matches a path exactly.
    pub fn matches_path_exact<T: AsRef<[u8]>>(&self, path: T) -> bool {
        self.matches_path_exact_do(path.as_ref())
    }

    fn matches_path_exact_do(&self, path: &[u8]) -> bool {
        match &self.pattern {
            MatchPattern::Pattern(pattern) => pattern.matches(path),
            MatchPattern::Literal(literal) => path == &literal[..],
        }
    }

    /// Check whether the path contains a matching suffix and the file mode match the expected file
    /// modes.
    ///
    /// This is a combination of using [`matches_mode()`](MatchEntry::matches_mode()) and
    /// [`matches_path_suffix()`](MatchEntry::matches_path_suffix()).
    pub fn matches<T: AsRef<[u8]>>(&self, path: T, file_mode: Option<u32>) -> bool {
        self.matches_do(path.as_ref(), file_mode)
    }

    fn matches_do(&self, path: &[u8], file_mode: Option<u32>) -> bool {
        if let Some(mode) = file_mode {
            if !self.matches_mode(mode) {
                return false;
            }
        }

        self.matches_path_suffix(path)
    }

    /// Check whether the path contains a matching suffix and the file mode match the expected file
    /// modes.
    ///
    /// This is a combination of using [`matches_mode()`](MatchEntry::matches_mode()) and
    /// [`matches_path_exact()`](MatchEntry::matches_path_exact()).
    pub fn matches_exact<T: AsRef<[u8]>>(&self, path: T, file_mode: Option<u32>) -> bool {
        self.matches_exact_do(path.as_ref(), file_mode)
    }

    fn matches_exact_do(&self, path: &[u8], file_mode: Option<u32>) -> bool {
        if let Some(mode) = file_mode {
            if !self.matches_mode(mode) {
                return false;
            }
        }

        self.matches_path_exact(path)
    }

    pub fn needs_file_mode(&self) -> bool {
        let flags = (self.flags & MatchFlag::ANY_FILE_TYPE).bits();
        flags != 0 && flags != MatchFlag::ANY_FILE_TYPE.bits()
    }
}

#[doc(hidden)]
pub trait MatchListEntry {
    fn entry_matches(&self, path: &[u8], file_mode: Option<u32>) -> Option<MatchType>;
    fn entry_matches_exact(&self, path: &[u8], file_mode: Option<u32>) -> Option<MatchType>;
    fn entry_needs_file_mode(&self) -> bool;
}

impl MatchListEntry for &'_ MatchEntry {
    fn entry_matches(&self, path: &[u8], file_mode: Option<u32>) -> Option<MatchType> {
        if self.matches(path, file_mode) {
            Some(self.match_type())
        } else {
            None
        }
    }

    fn entry_matches_exact(&self, path: &[u8], file_mode: Option<u32>) -> Option<MatchType> {
        if self.matches_exact(path, file_mode) {
            Some(self.match_type())
        } else {
            None
        }
    }

    fn entry_needs_file_mode(&self) -> bool {
        self.needs_file_mode()
    }
}

impl MatchListEntry for &'_ &'_ MatchEntry {
    fn entry_matches(&self, path: &[u8], file_mode: Option<u32>) -> Option<MatchType> {
        if self.matches(path, file_mode) {
            Some(self.match_type())
        } else {
            None
        }
    }

    fn entry_matches_exact(&self, path: &[u8], file_mode: Option<u32>) -> Option<MatchType> {
        if self.matches_exact(path, file_mode) {
            Some(self.match_type())
        } else {
            None
        }
    }

    fn entry_needs_file_mode(&self) -> bool {
        self.needs_file_mode()
    }
}

/// This provides [`matches`](MatchList::matches) and [`matches_exact`](MatchList::matches_exact)
/// methods to lists of [`MatchEntry`] items.
///
/// Technically this is implemented for anything that you can turn into a
/// [`DoubleEndedIterator`](std::iter::DoubleEndedIterator) over [`MatchEntry`] or
/// [`&MatchEntry`](MatchEntry).
///
/// In practice this means you can use it with slices or references to [`Vec`](std::vec::Vec) or
/// [`VecDeque`](std::collections::VecDeque) etc.
///
/// This makes it easier to use slices over entries or references to entries.
pub trait MatchList {
    /// Check whether this list contains anything matching a prefix of the specified path, and the
    /// specified file mode. Gets the file_mode lazily, only if needed.
    fn matches<P, U>(&self, path: P, get_file_mode: U) -> Result<Option<MatchType>, U::Error>
    where
        P: AsRef<[u8]>,
        U: GetFileMode;

    /// Check whether this list contains anything exactly matching the path and mode. Gets the
    /// file_mode lazily, only if needed.
    fn matches_exact<P, U>(&self, path: P, get_file_mode: U) -> Result<Option<MatchType>, U::Error>
    where
        P: AsRef<[u8]>,
        U: GetFileMode;
}

impl<'a, T> MatchList for T
where
    T: 'a + ?Sized,
    &'a T: IntoIterator,
    <&'a T as IntoIterator>::IntoIter: DoubleEndedIterator,
    <&'a T as IntoIterator>::Item: MatchListEntry,
{
    fn matches<P, G>(&self, path: P, get_file_mode: G) -> Result<Option<MatchType>, G::Error>
    where
        P: AsRef<[u8]>,
        G: GetFileMode,
    {
        // This is an &self method on a `T where T: 'a`.
        let this: &'a Self = unsafe { std::mem::transmute(self) };

        let mut get_file_mode = Some(get_file_mode);
        let mut file_mode = None;

        for m in this.into_iter().rev() {
            if file_mode.is_none() && m.entry_needs_file_mode() {
                file_mode = Some(get_file_mode.take().unwrap().get()?);
            }
            if let Some(mt) = m.entry_matches(path.as_ref(), file_mode) {
                return Ok(Some(mt));
            }
        }
        Ok(None)
    }

    fn matches_exact<P, G>(&self, path: P, get_file_mode: G) -> Result<Option<MatchType>, G::Error>
    where
        P: AsRef<[u8]>,
        G: GetFileMode,
    {
        // This is an &self method on a `T where T: 'a`.
        let this: &'a Self = unsafe { std::mem::transmute(self) };

        let mut get_file_mode = Some(get_file_mode);
        let mut file_mode = None;

        for m in this.into_iter().rev() {
            if file_mode.is_none() && m.entry_needs_file_mode() {
                file_mode = Some(get_file_mode.take().unwrap().get()?);
            }
            if let Some(mt) = m.entry_matches_exact(path.as_ref(), file_mode) {
                return Ok(Some(mt));
            }
        }
        Ok(None)
    }
}

#[test]
fn assert_containers_implement_match_list() {
    use std::iter::FromIterator;

    let vec = vec![MatchEntry::include(crate::Pattern::path("a*").unwrap())];
    assert_eq!(vec.matches("asdf", None), Ok(Some(MatchType::Include)));

    // FIXME: ideally we can make this work as well!
    let vd = std::collections::VecDeque::<MatchEntry>::from_iter(vec.clone());
    assert_eq!(vd.matches("asdf", None), Ok(Some(MatchType::Include)));

    let list: &[MatchEntry] = &vec[..];
    assert_eq!(list.matches("asdf", None), Ok(Some(MatchType::Include)));

    let list: Vec<&MatchEntry> = vec.iter().collect();
    assert_eq!(list.matches("asdf", None), Ok(Some(MatchType::Include)));

    let list: &[&MatchEntry] = &list[..];
    assert_eq!(list.matches("asdf", None), Ok(Some(MatchType::Include)));
}

#[test]
fn test_file_type_matches() {
    let matchlist = vec![
        MatchEntry::parse_pattern("a_dir/", PatternFlag::PATH_NAME, MatchType::Include).unwrap(),
        MatchEntry::parse_pattern("!a_file", PatternFlag::PATH_NAME, MatchType::Include)
            .unwrap()
            .flags(MatchFlag::MATCH_REGULAR_FILES),
        MatchEntry::parse_pattern("!another_dir//", PatternFlag::PATH_NAME, MatchType::Include)
            .unwrap(),
    ];
    assert_eq!(
        matchlist.matches("a_dir", Some(libc::S_IFDIR)),
        Ok(Some(MatchType::Include))
    );
    assert_eq!(
        matchlist.matches("/a_dir", Some(libc::S_IFDIR)),
        Ok(Some(MatchType::Include))
    );
    assert_eq!(matchlist.matches("/a_dir", Some(libc::S_IFREG)), Ok(None));

    assert_eq!(
        matchlist.matches("/a_file", Some(libc::S_IFREG)),
        Ok(Some(MatchType::Exclude))
    );
    assert_eq!(matchlist.matches("/a_file", Some(libc::S_IFDIR)), Ok(None));

    assert_eq!(
        matchlist.matches("/another_dir", Some(libc::S_IFDIR)),
        Ok(Some(MatchType::Exclude))
    );
    assert_eq!(
        matchlist.matches("/another_dir", Some(libc::S_IFREG)),
        Ok(None)
    );
}

#[test]
fn test_anchored_matches() {
    use crate::Pattern;

    let matchlist = vec![
        MatchEntry::new(Pattern::path("file-a").unwrap(), MatchType::Include),
        MatchEntry::new(Pattern::path("some/path").unwrap(), MatchType::Include)
            .add_flags(MatchFlag::ANCHORED),
    ];

    assert_eq!(
        matchlist.matches("file-a", None),
        Ok(Some(MatchType::Include))
    );
    assert_eq!(
        matchlist.matches("another/file-a", None),
        Ok(Some(MatchType::Include))
    );

    assert_eq!(matchlist.matches("some", None), Ok(None));
    assert_eq!(matchlist.matches("path", None), Ok(None));
    assert_eq!(
        matchlist.matches("some/path", None),
        Ok(Some(MatchType::Include))
    );
    assert_eq!(matchlist.matches("another/some/path", None), Ok(None));
}

#[test]
fn test_literal_matches() {
    let matchlist = vec![MatchEntry::new(
        MatchPattern::Literal(b"/bin/mv".to_vec()),
        MatchType::Include,
    )];
    assert_eq!(
        matchlist.matches("/bin/mv", None),
        Ok(Some(MatchType::Include))
    );
}

#[test]
fn test_path_relativity() {
    use crate::Pattern;
    let matchlist = vec![
        MatchEntry::new(Pattern::path("noslash").unwrap(), MatchType::Include),
        MatchEntry::new(Pattern::path("noslash-a").unwrap(), MatchType::Include)
            .add_flags(MatchFlag::ANCHORED),
        MatchEntry::new(Pattern::path("/slash").unwrap(), MatchType::Include),
        MatchEntry::new(Pattern::path("/slash-a").unwrap(), MatchType::Include)
            .add_flags(MatchFlag::ANCHORED),
    ];
    assert_eq!(
        matchlist.matches("noslash", None),
        Ok(Some(MatchType::Include))
    );
    assert_eq!(
        matchlist.matches("noslash-a", None),
        Ok(Some(MatchType::Include))
    );
    assert_eq!(matchlist.matches("slash", None), Ok(None));
    assert_eq!(
        matchlist.matches("/slash", None),
        Ok(Some(MatchType::Include))
    );
    assert_eq!(matchlist.matches("slash-a", None), Ok(None));
    assert_eq!(
        matchlist.matches("/slash-a", None),
        Ok(Some(MatchType::Include))
    );

    assert_eq!(
        matchlist.matches("foo/noslash", None),
        Ok(Some(MatchType::Include))
    );
    assert_eq!(matchlist.matches("foo/noslash-a", None), Ok(None));
    assert_eq!(matchlist.matches("foo/slash", None), Ok(None));
    assert_eq!(matchlist.matches("foo/slash-a", None), Ok(None));
}

#[test]
fn matches_path() {
    use crate::Pattern;

    let matchlist = vec![
        MatchEntry::new(Pattern::path("a*").unwrap(), MatchType::Exclude),
        MatchEntry::new(Pattern::path("b*").unwrap(), MatchType::Exclude),
    ];

    assert_eq!(
        matchlist.matches("ahsjdj", || Err(FileModeRequired)),
        Ok(Some(MatchType::Exclude))
    );
    let mut test = 1;
    let result = matchlist.matches("bhshdf", || {
        test += 1;
        Ok::<u32, FileModeRequired>(libc::S_IFDIR)
    });
    assert_eq!(result, Ok(Some(MatchType::Exclude)));
    assert_eq!(test, 1);

    let matchlist = vec![
        MatchEntry::new(Pattern::path("a*").unwrap(), MatchType::Exclude)
            .flags(MatchFlag::MATCH_DIRECTORIES),
        MatchEntry::new(Pattern::path("b*").unwrap(), MatchType::Exclude),
    ];

    let mut test = 1;
    let result = matchlist.matches("aa", || {
        test += 1;
        Ok::<u32, FileModeRequired>(libc::S_IFDIR)
    });
    assert_eq!(result, Ok(Some(MatchType::Exclude)));
    assert_eq!(test, 2);
    assert_eq!(
        matchlist.matches("ahsjdj", || Err(FileModeRequired)),
        Err(FileModeRequired)
    );
    assert_eq!(
        matchlist.matches("bhshdf", || Err(FileModeRequired)),
        Ok(Some(MatchType::Exclude))
    );
    assert_eq!(
        matchlist.matches("ahsjdj", || Ok::<u32, FileModeRequired>(libc::S_IFREG)),
        Ok(None)
    );

    let matchlist = vec![
        MatchEntry::new(Pattern::path("b*").unwrap(), MatchType::Include),
        MatchEntry::new(Pattern::path("a*").unwrap(), MatchType::Exclude)
            .flags(MatchFlag::MATCH_DIRECTORIES),
    ];
    assert_eq!(
        matchlist.matches("ahsjdj", || Err(FileModeRequired)),
        Err(FileModeRequired)
    );
    assert_eq!(
        matchlist.matches("bhshdf", || Err(FileModeRequired)),
        Err(FileModeRequired)
    );
    assert_eq!(
        matchlist.matches("ahsjdj", || Ok::<u32, FileModeRequired>(libc::S_IFDIR)),
        Ok(Some(MatchType::Exclude))
    );
    assert_eq!(
        matchlist.matches("ahsjdj", || Ok::<u32, FileModeRequired>(libc::S_IFREG)),
        Ok(None)
    );
    assert_eq!(
        matchlist.matches("bbb", || Ok::<u32, FileModeRequired>(libc::S_IFREG)),
        Ok(Some(MatchType::Include))
    );

    let matchlist = vec![
        MatchEntry::new(Pattern::path("a*").unwrap(), MatchType::Exclude)
            .flags(MatchFlag::MATCH_DIRECTORIES),
    ];

    assert_eq!(
        matchlist.matches("bbb", || Ok::<u32, FileModeRequired>(libc::S_IFDIR)),
        Ok(None)
    );

    let matchlist = vec![
        MatchEntry::new(Pattern::path("a*").unwrap(), MatchType::Exclude)
            .flags(MatchFlag::MATCH_DIRECTORIES),
        MatchEntry::new(Pattern::path("b*").unwrap(), MatchType::Exclude)
            .flags(MatchFlag::MATCH_REGULAR_FILES),
    ];

    assert_eq!(
        matchlist.matches("ahsjdj", || Ok::<u32, FileModeRequired>(libc::S_IFDIR)),
        Ok(Some(MatchType::Exclude))
    );
    assert_eq!(
        matchlist.matches("ahsjdj", || Ok::<u32, FileModeRequired>(libc::S_IFREG)),
        Ok(None)
    );
    assert_eq!(
        matchlist.matches("bhsjdj", || Ok::<u32, FileModeRequired>(libc::S_IFREG)),
        Ok(Some(MatchType::Exclude))
    );
    assert_eq!(
        matchlist.matches("bhsjdj", libc::S_IFREG as u32),
        Ok(Some(MatchType::Exclude))
    );
    assert_eq!(matchlist.matches("bhsjdj", 0), Ok(None));
}

#[test]
fn match_entry_needs_flag() {
    use crate::Pattern;
    let match_entry =
        MatchEntry::parse_pattern("a*/", PatternFlag::PATH_NAME, MatchType::Exclude).unwrap();
    assert_eq!(match_entry.needs_file_mode(), true);

    let match_entry = MatchEntry::new(Pattern::path("a*").unwrap(), MatchType::Exclude)
        .flags(MatchFlag::MATCH_REGULAR_FILES);
    assert_eq!(match_entry.needs_file_mode(), true);

    let match_entry = MatchEntry::new(Pattern::path("a*").unwrap(), MatchType::Exclude)
        .flags(MatchFlag::MATCH_DIRECTORIES | MatchFlag::MATCH_REGULAR_FILES);
    assert_eq!(match_entry.needs_file_mode(), true);

    let match_entry = MatchEntry::new(Pattern::path("a*").unwrap(), MatchType::Exclude)
        .flags(MatchFlag::ANCHORED | MatchFlag::MATCH_DIRECTORIES);
    assert_eq!(match_entry.needs_file_mode(), true);

    let match_entry = MatchEntry::new(Pattern::path("a*").unwrap(), MatchType::Exclude)
        .flags(MatchFlag::ANY_FILE_TYPE);
    assert_eq!(match_entry.needs_file_mode(), false);
}
