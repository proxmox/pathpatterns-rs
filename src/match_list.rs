//! Helpers for include/exclude lists.

use bitflags::bitflags;

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
}

impl From<crate::Pattern> for MatchPattern {
    fn from(pattern: crate::Pattern) -> Self {
        MatchPattern::Pattern(pattern)
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

/// A single entry in a `MatchList`.
#[derive(Clone, Debug)]
pub struct MatchEntry {
    pattern: MatchPattern,
    ty: MatchType,
    flags: MatchFlag,
}

impl MatchEntry {
    /// Create a new match entry.
    pub fn new<T: Into<MatchPattern>>(pattern: T, ty: MatchType, flags: MatchFlag) -> Self {
        Self {
            pattern: pattern.into(),
            ty,
            flags,
        }
    }

    /// Create a new include-type match entry with default flags.
    pub fn include<T: Into<MatchPattern>>(pattern: T) -> Self {
        Self::new(pattern.into(), MatchType::Include, MatchFlag::default())
    }

    /// Create a new exclude-type match entry with default flags.
    pub fn exclude<T: Into<MatchPattern>>(pattern: T) -> Self {
        Self::new(pattern.into(), MatchType::Exclude, MatchFlag::default())
    }

    #[inline]
    pub fn match_type(&self) -> MatchType {
        self.ty
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

        if path[0] != b'/' {
            // we had "foo/bar", so we haven't yet tried to match the whole string:
            self.matches_path_exact(path)
        } else {
            false
        }
    }

    /// Test whether this entry's pattern matches a path exactly.
    pub fn matches_path_exact<T: AsRef<[u8]>>(&self, path: T) -> bool {
        self.matches_path_exact_do(path.as_ref())
    }

    fn matches_path_exact_do(&self, path: &[u8]) -> bool {
        match &self.pattern {
            MatchPattern::Pattern(pattern) => pattern.matches(path),
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

/// Convenience type for an ordered list of `MatchEntry`s. This is just a `Vec<MatchEntry>`.
#[derive(Clone, Debug, Default)]
pub struct MatchList {
    list: Vec<MatchEntry>,
}

impl MatchList {
    pub fn new<T: Into<Vec<MatchEntry>>>(list: T) -> Self {
        Self { list: list.into() }
    }

    pub fn push(&mut self, entry: MatchEntry) {
        self.list.push(entry)
    }

    pub fn pop(&mut self) -> Option<MatchEntry> {
        self.list.pop()
    }
}

impl From<Vec<MatchEntry>> for MatchList {
    fn from(list: Vec<MatchEntry>) -> Self {
        Self { list }
    }
}

impl std::ops::Deref for MatchList {
    type Target = MatchListRef;

    fn deref(&self) -> &Self::Target {
        (&self.list[..]).into()
    }
}

/// Helper to provide the `matches` method on slices of `MatchEntry`s.
#[repr(transparent)]
pub struct MatchListRef([MatchEntry]);

impl std::ops::Deref for MatchListRef {
    type Target = [MatchEntry];

    fn deref(&self) -> &Self::Target {
        &self.0[..]
    }
}

impl<'a> From<&'a [MatchEntry]> for &'a MatchListRef {
    fn from(entries: &'a [MatchEntry]) -> &'a MatchListRef {
        unsafe { &*(entries as *const [MatchEntry] as *const MatchListRef) }
    }
}

impl MatchListRef {
    /// Check whether this list contains anything matching a prefix of the specified path, and the
    /// specified file mode.
    pub fn matches<T: AsRef<[u8]>>(&self, path: T, file_mode: Option<u32>) -> Option<MatchType> {
        self.matches_do(path.as_ref(), file_mode)
    }

    fn matches_do(&self, path: &[u8], file_mode: Option<u32>) -> Option<MatchType> {
        for m in self.iter().rev() {
            if m.matches(path, file_mode) {
                return Some(m.match_type());
            }
        }

        None
    }

    /// Check whether this list contains anything exactly matching the path and mode.
    pub fn matches_exact<T: AsRef<[u8]>>(
        &self,
        path: T,
        file_mode: Option<u32>,
    ) -> Option<MatchType> {
        self.matches_exact_do(path.as_ref(), file_mode)
    }

    fn matches_exact_do(&self, path: &[u8], file_mode: Option<u32>) -> Option<MatchType> {
        for m in self.iter().rev() {
            if m.matches_exact(path, file_mode) {
                return Some(m.match_type());
            }
        }

        None
    }
}
