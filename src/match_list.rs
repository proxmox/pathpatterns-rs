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

/// A pattern entry. For now this only contains glob patterns, but we may want to add regex
/// patterns or user defined callback functions later on as well.
///
/// For regex we'd likely use the POSIX extended REs via `regexec(3)`, since we're targetting
/// command line interfaces and want something command line users are used to.
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

/// A pattern can be used as an include or an exclude pattern. In a list of `MatchEntry`s, later
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

/// A single entry in a `MatchList`.
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

    /// Parse a pattern into a `MatchEntry` while interpreting a leading exclamation mark as
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
        let (pattern, ty) = if pattern.get(0).copied() == Some(b'!') {
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
        if self.flags.contains(MatchFlag::ANY_FILE_TYPE) {
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

    /// Check whether the path contains a matching suffix and the file mode match the expected file modes.
    /// This is a combination of using `.matches_mode()` and `.matches_path_suffix()`.
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

    /// Check whether the path contains a matching suffix and the file mode match the expected file modes.
    /// This is a combination of using `.matches_mode()` and `.matches_path_exact()`.
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
}

#[doc(hidden)]
pub trait MatchListEntry {
    fn entry_matches(&self, path: &[u8], file_mode: Option<u32>) -> Option<MatchType>;
    fn entry_matches_exact(&self, path: &[u8], file_mode: Option<u32>) -> Option<MatchType>;
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
}

/// This provides `matches` and `matches_exact` methods to lists of `MatchEntry`s.
///
/// Technically this is implemented for anything you can turn into a `DoubleEndedIterator` over
/// `MatchEntry` or `&MatchEntry`.
///
/// In practice this means you can use it with slices or references to `Vec` or `VecDeque` etc.
/// This makes it easier to use slices over entries or references to entries.
pub trait MatchList {
    /// Check whether this list contains anything matching a prefix of the specified path, and the
    /// specified file mode.
    fn matches<T: AsRef<[u8]>>(&self, path: T, file_mode: Option<u32>) -> Option<MatchType> {
        self.matches_do(path.as_ref(), file_mode)
    }

    fn matches_do(&self, path: &[u8], file_mode: Option<u32>) -> Option<MatchType>;

    /// Check whether this list contains anything exactly matching the path and mode.
    fn matches_exact<T: AsRef<[u8]>>(
        &self,
        path: T,
        file_mode: Option<u32>,
    ) -> Option<MatchType> {
        self.matches_exact_do(path.as_ref(), file_mode)
    }

    fn matches_exact_do(&self, path: &[u8], file_mode: Option<u32>) -> Option<MatchType>;
}

impl<'a, T> MatchList for T
where
    T: 'a + ?Sized,
    &'a T: IntoIterator,
    <&'a T as IntoIterator>::IntoIter: DoubleEndedIterator,
    <&'a T as IntoIterator>::Item: MatchListEntry,
{
    fn matches_do(&self, path: &[u8], file_mode: Option<u32>) -> Option<MatchType> {
        // This is an &self method on a `T where T: 'a`.
        let this: &'a Self = unsafe { std::mem::transmute(self) };

        for m in this.into_iter().rev() {
            if let Some(mt) = m.entry_matches(path, file_mode) {
                return Some(mt);
            }
        }

        None
    }

    fn matches_exact_do(&self, path: &[u8], file_mode: Option<u32>) -> Option<MatchType> {
        // This is an &self method on a `T where T: 'a`.
        let this: &'a Self = unsafe { std::mem::transmute(self) };

        for m in this.into_iter().rev() {
            if let Some(mt) = m.entry_matches_exact(path, file_mode) {
                return Some(mt);
            }
        }

        None
    }
}

#[test]
fn assert_containers_implement_match_list() {
    use std::iter::FromIterator;

    let vec = vec![MatchEntry::include(crate::Pattern::path("a*").unwrap())];
    assert_eq!(vec.matches("asdf", None), Some(MatchType::Include));

    // FIXME: ideally we can make this work as well!
    let vd = std::collections::VecDeque::<MatchEntry>::from_iter(vec.clone());
    assert_eq!(vd.matches("asdf", None), Some(MatchType::Include));

    let list: &[MatchEntry] = &vec[..];
    assert_eq!(list.matches("asdf", None), Some(MatchType::Include));

    let list: Vec<&MatchEntry> = vec.iter().collect();
    assert_eq!(list.matches("asdf", None), Some(MatchType::Include));

    let list: &[&MatchEntry] = &list[..];
    assert_eq!(list.matches("asdf", None), Some(MatchType::Include));
}

#[test]
fn test_file_type_matches() {
    let matchlist = vec![
        MatchEntry::parse_pattern("a_dir/", PatternFlag::PATH_NAME, MatchType::Include)
            .unwrap(),
        MatchEntry::parse_pattern("!a_file", PatternFlag::PATH_NAME, MatchType::Include)
            .unwrap()
            .flags(MatchFlag::MATCH_REGULAR_FILES),
        MatchEntry::parse_pattern("!another_dir//", PatternFlag::PATH_NAME, MatchType::Include)
            .unwrap(),
    ];
    assert_eq!(
        matchlist.matches("a_dir", Some(libc::S_IFDIR)),
        Some(MatchType::Include)
    );
    assert_eq!(
        matchlist.matches("/a_dir", Some(libc::S_IFDIR)),
        Some(MatchType::Include)
    );
    assert_eq!(matchlist.matches("/a_dir", Some(libc::S_IFREG)), None);

    assert_eq!(
        matchlist.matches("/a_file", Some(libc::S_IFREG)),
        Some(MatchType::Exclude)
    );
    assert_eq!(matchlist.matches("/a_file", Some(libc::S_IFDIR)), None);

    assert_eq!(
        matchlist.matches("/another_dir", Some(libc::S_IFDIR)),
        Some(MatchType::Exclude)
    );
    assert_eq!(matchlist.matches("/another_dir", Some(libc::S_IFREG)), None);
}

#[test]
fn test_anchored_matches() {
    use crate::Pattern;

    let matchlist = vec![
        MatchEntry::new(Pattern::path("file-a").unwrap(), MatchType::Include),
        MatchEntry::new(Pattern::path("some/path").unwrap(), MatchType::Include)
            .flags(MatchFlag::ANCHORED),
    ];

    assert_eq!(matchlist.matches("file-a", None), Some(MatchType::Include));
    assert_eq!(
        matchlist.matches("another/file-a", None),
        Some(MatchType::Include)
    );

    assert_eq!(matchlist.matches("some", None), None);
    assert_eq!(matchlist.matches("path", None), None);
    assert_eq!(
        matchlist.matches("some/path", None),
        Some(MatchType::Include)
    );
    assert_eq!(matchlist.matches("another/some/path", None), None);
}

#[test]
fn test_literal_matches() {
    let matchlist = vec![
        MatchEntry::new(MatchPattern::Literal(b"/bin/mv".to_vec()), MatchType::Include),
    ];
    assert_eq!(matchlist.matches("/bin/mv", None), Some(MatchType::Include));
}
