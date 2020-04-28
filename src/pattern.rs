//! This implements the pattern matching algorithm found in git's `wildmatch.c`

use std::fmt;
use std::mem;
use std::ops::RangeInclusive;

use bitflags::bitflags;

/// A character class can be a list of characters `[abc]`, ranges of characters `[a-z]`, or named
/// classes `[[:digit:]]`, or a combination of them all. Additionally they can be negated with a
/// `^` at the beginning.
#[derive(Clone, Debug, Default)]
struct CharacterClass {
    negated: bool,
    named: Vec<fn(u8) -> bool>,
    listed: Vec<u8>,
    ranges: Vec<RangeInclusive<u8>>,
}

impl CharacterClass {
    /// Check if a byte match this character class.
    pub fn matches(&self, ch: u8) -> bool {
        self.matches_do(ch) != self.negated
    }

    fn matches_do(&self, ch: u8) -> bool {
        self.listed.contains(&ch)
            || self.ranges.iter().any(|range| range.contains(&ch))
            || self.named.iter().any(|func| func(ch))
    }
}

/// One component of a pattern.
#[derive(Clone, Debug)]
enum Component {
    /// A literal match. The `/a` and `.txt` in `/a*.txt`.
    Literal(Vec<u8>),

    /// A question mark should match exactly one byte. If desired it may also match slashes, but
    /// this property is part of the whole pattern, not the component.
    QuestionMark,

    /// A "star" normally matches everything except when matching path names, where it does not
    /// match slashes.
    Star,

    /// A double star always matches everything, even slashes.
    StarStar,

    // SlashStarStarSlash, // maybe?
    /// A character class matches one byte out of a set of allowed or disallowed bytes.
    Class(CharacterClass),
}

impl Component {
    /// Check if this is a literal component ending with a slash.
    fn ends_with_slash(&self) -> bool {
        match self {
            Component::Literal(lit) => lit.last().copied() == Some(b'/'),
            _ => false,
        }
    }

    /// Check if this is a literal component starting with a slash.
    fn starts_with_slash(&self) -> bool {
        match self {
            Component::Literal(lit) => {
                lit.first().copied() == Some(b'/')
                    || (lit.first().copied() == Some(b'\\') && lit.get(1).copied() == Some(b'/'))
            }
            _ => false,
        }
    }
}

bitflags! {
    /// Flags affecting how a pattern should match.
    pub struct PatternFlag: u8 {
        /// Ignore upper/lower case on the pattern. Note that this only affects ascii characters.
        /// We do not normalize/casefold unicode here. If you need this, case-fold your input
        /// strings and patterns first.
        const IGNORE_CASE = 0x01;

        /// This pattern is used for paths, meaning that `*` and `?` do not match slashes. Only
        /// explicit slashes and `**` can match slashes.
        const PATH_NAME = 0x02;
    }
}

/// Error cases which may happen while parsing a pattern.
#[derive(Clone, Debug)]
pub enum ParseError {
    EmptyPattern,
    NulByteError,
    TrailingBackslash,
    UnclosedCharacterClass(usize),
    MalformedNamedCharacterClass(usize),
}

impl std::error::Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::EmptyPattern => write!(f, "empty pattern"),
            ParseError::NulByteError => write!(f, "null-byte in pattern"),
            ParseError::TrailingBackslash => write!(f, "trailing backslash in pattern"),
            ParseError::UnclosedCharacterClass(begin) => write!(
                f,
                "unclosed character class in pattern, starting at byte {}",
                begin
            ),
            ParseError::MalformedNamedCharacterClass(begin) => write!(
                f,
                "malformed named character class in pattern, starting at byte {}",
                begin
            ),
        }
    }
}

/// See the match function for the algorithm.
enum MatchResult {
    Match,
    NoMatch,
    AbortAll,
    AbortToStarStar,
}

/// An `fnmatch`-like pattern working like in `git`, with support for `*` vs `**` distinction for
/// paths.
///
/// Note that patterns are treated as ASCII strings, so unicode characters have no special effect.
/// You can use it for UTF-8 paths, but there's no direct support for `PatternFlag::IGNORE_CASE` on
/// unicode text.
#[derive(Clone, Debug)]
pub struct Pattern {
    /// Original pattern the user provided.
    pattern: std::ffi::CString,

    /// Matching components we parsed out of the string.
    components: Vec<Component>,

    /// Flags used for the pattern.
    ///
    /// In the future we may want to optimize the `components` based on this, so `IGNORE_CASE`
    /// happens at "compile time" already.
    flags: PatternFlag,
}

impl Pattern {
    /// Get the original input pattern.
    pub fn pattern(&self) -> &std::ffi::CStr {
        &self.pattern
    }

    /// Create a new pattern.
    pub fn new<T: AsRef<[u8]>>(pattern: T, flags: PatternFlag) -> Result<Self, ParseError> {
        Self::new_do(pattern.as_ref(), flags)
    }

    /// Convenience shortcut to create a new pattern with `PatternFlag::PATH_NAME`.
    pub fn path<T: AsRef<[u8]>>(pattern: T) -> Result<Self, ParseError> {
        Self::new_do(pattern.as_ref(), PatternFlag::PATH_NAME)
    }

    fn new_do(pattern: &[u8], flags: PatternFlag) -> Result<Self, ParseError> {
        if pattern.is_empty() {
            return Err(ParseError::EmptyPattern);
        }

        // strip trailing slashes:
        let pattern = match pattern.iter().rposition(|&b| b != b'/') {
            Some(pos) => &pattern[..=pos],
            None => b"/",
        };

        let c_pattern = std::ffi::CString::new(pattern).map_err(|_| ParseError::NulByteError)?;

        let mut components = Vec::<Component>::new();
        let mut literal = Vec::<u8>::new();

        fn push_literal(
            literal: &mut Vec<u8>,
            components: &mut Vec<Component>,
            flags: PatternFlag,
        ) {
            if !literal.is_empty() {
                if flags.intersects(PatternFlag::IGNORE_CASE) {
                    for b in &mut literal[..] {
                        *b = b.to_ascii_lowercase();
                    }
                }
                components.push(Component::Literal(mem::take(literal)));
            }
        }

        let mut i = 0;
        while i != pattern.len() {
            match pattern[i] {
                0 => return Err(ParseError::NulByteError),
                b'\\' => {
                    i += 1;
                    let mut ch = *pattern.get(i).ok_or(ParseError::TrailingBackslash)?;
                    if flags.intersects(PatternFlag::IGNORE_CASE) {
                        ch = ch.to_ascii_lowercase()
                    }
                    literal.push(ch);
                }
                b'?' => {
                    push_literal(&mut literal, &mut components, flags);
                    components.push(Component::QuestionMark);
                }
                b'*' => {
                    push_literal(&mut literal, &mut components, flags);
                    if pattern.get(i + 1).copied() == Some(b'*') {
                        let beg = i;
                        i += 1;
                        // swallow following stars as well:
                        while pattern.get(i + 1).copied() == Some(b'*') {
                            i += 1;
                        }

                        // git doesn't allow `**` attached to anything other than slashes to match
                        // subdirectories, so only `**`, `.../**`, `.../**/...` and `**/...` are
                        // valid.
                        if (beg == 0 || pattern[beg - 1] == b'/')
                            && ((i + 1) == pattern.len() || pattern[i + 1] == b'/')
                        {
                            components.push(Component::StarStar)
                        } else {
                            components.push(Component::Star)
                        }
                    } else {
                        components.push(Component::Star)
                    }
                }
                b'[' => {
                    push_literal(&mut literal, &mut components, flags);
                    let (component, new_i) = Self::parse_char_class(pattern, i, flags)?;
                    i = new_i;
                    components.push(component);
                }
                ch => literal.push(if flags.intersects(PatternFlag::IGNORE_CASE) {
                    ch.to_ascii_lowercase()
                } else {
                    ch
                }),
            }

            i += 1;
        }

        push_literal(&mut literal, &mut components, flags);
        Ok(Self {
            pattern: c_pattern,
            components,
            flags,
        })
    }

    fn parse_char_class(
        pattern: &[u8],
        begin_i: usize,
        flags: PatternFlag,
    ) -> Result<(Component, usize), ParseError> {
        let mut i = begin_i + 1;

        let negated = if pattern.get(i).copied() == Some(b'^') {
            i += 1;
            true
        } else {
            false
        };

        if i == pattern.len() {
            return Err(ParseError::UnclosedCharacterClass(begin_i));
        }

        let mut class = CharacterClass {
            negated,
            ..Default::default()
        };
        let mut prev = None;
        while i != pattern.len() {
            let mut new_prev = None;
            match pattern[i] {
                0 => return Err(ParseError::NulByteError),
                b'[' if pattern[(i + 1)..].starts_with(b":alnum:]") => {
                    i += 8;
                    class.named.push(|b| b.is_ascii_alphanumeric());
                }
                b'[' if pattern[(i + 1)..].starts_with(b":alpha:]") => {
                    i += 8;
                    class.named.push(|b| b.is_ascii_alphabetic());
                }
                b'[' if pattern[(i + 1)..].starts_with(b":blank:]") => {
                    i += 8;
                    class.named.push(|b| b == b' ' || b == b'\t');
                }
                b'[' if pattern[(i + 1)..].starts_with(b":cntrl:]") => {
                    i += 8;
                    class.named.push(|b| b.is_ascii_control());
                }
                b'[' if pattern[(i + 1)..].starts_with(b":digit:]") => {
                    i += 8;
                    class.named.push(|b| b.is_ascii_digit());
                }
                b'[' if pattern[(i + 1)..].starts_with(b":graph:]") => {
                    i += 8;
                    class.named.push(|b| b.is_ascii_graphic());
                }
                b'[' if pattern[(i + 1)..].starts_with(b":lower:]") => {
                    i += 8;
                    class.named.push(|b| b.is_ascii_lowercase());
                }
                b'[' if pattern[(i + 1)..].starts_with(b":print:]") => {
                    i += 8;
                    class.named.push(|b| b >= 0x20 && b <= 0x7f);
                }
                b'[' if pattern[(i + 1)..].starts_with(b":punct:]") => {
                    i += 8;
                    class.named.push(|b| b.is_ascii_punctuation());
                }
                b'[' if pattern[(i + 1)..].starts_with(b":space:]") => {
                    i += 8;
                    class.named.push(|b| b.is_ascii_whitespace());
                }
                b'[' if pattern[(i + 1)..].starts_with(b":upper:]") => {
                    i += 8;
                    if flags.intersects(PatternFlag::IGNORE_CASE) {
                        class.named.push(|b| b.is_ascii_lowercase());
                    } else {
                        class.named.push(|b| b.is_ascii_uppercase());
                    }
                }
                b'[' if pattern[(i + 1)..].starts_with(b":xdigit:]") => {
                    i += 9;
                    class.named.push(|b| b.is_ascii_hexdigit());
                }
                b'[' if pattern.get(i + 1).copied() == Some(b':') => {
                    return Err(ParseError::MalformedNamedCharacterClass(begin_i));
                }
                b']' => break,
                b'\\' => {
                    i += 1;
                    let ch = *pattern.get(i).ok_or(ParseError::TrailingBackslash)?;
                    class.listed.push(ch);
                    new_prev = Some(ch);
                }
                b'-' => match prev {
                    None => {
                        new_prev = Some(b'-');
                        class.listed.push(b'-');
                    }
                    Some(beg) => {
                        // The previous character was also pushed to `class.listed`, so remove it:
                        class.listed.pop();

                        i += 1;
                        let mut end = *pattern
                            .get(i)
                            .ok_or(ParseError::UnclosedCharacterClass(begin_i))?;
                        if end == b'\\' {
                            i += 1;
                            end = *pattern
                                .get(i)
                                .ok_or(ParseError::UnclosedCharacterClass(begin_i))?;
                        }

                        if flags.intersects(PatternFlag::IGNORE_CASE) {
                            end = end.to_ascii_lowercase();
                        }

                        if beg <= end {
                            class.ranges.push(beg..=end);
                        } else {
                            class.ranges.push(end..=beg);
                        }
                    }
                },
                mut ch => {
                    if flags.intersects(PatternFlag::IGNORE_CASE) {
                        ch = ch.to_ascii_lowercase();
                    }
                    new_prev = Some(ch);
                    class.listed.push(ch);
                }
            }
            prev = new_prev;
            i += 1;
        }

        Ok((Component::Class(class), i))
    }

    /// Check whether this pattern matches a text.
    pub fn matches<T: AsRef<[u8]>>(&self, text: T) -> bool {
        match self.do_matches(&self.components, 0, text.as_ref()) {
            MatchResult::Match => true,
            _ => false,
        }
    }

    // The algorithm is ported from git's wildmatch.c.
    fn do_matches(&self, components: &[Component], mut ci: usize, mut text: &[u8]) -> MatchResult {
        if self.flags.intersects(PatternFlag::PATH_NAME) {
            // If we match a path then we want the pattern `"foo"` to match the path `"/foo"`.
        }

        while ci != components.len() {
            //eprintln!("Matching: {:?} at text: {:?}", components[ci], unsafe {
            //    std::str::from_utf8_unchecked(text)
            //},);
            match &components[ci] {
                Component::Literal(literal) => {
                    if text.is_empty() {
                        // The '*' implementation is NON-greedy, so if the text is empty, we
                        // already tried all shorter possible matches, so anything other than a '*'
                        // match can `AbortAll` if the text is empty.
                        return MatchResult::AbortAll;
                    }

                    if !starts_with(text, &literal, self.flags) {
                        return MatchResult::NoMatch;
                    }

                    text = &text[literal.len()..];
                }
                Component::QuestionMark => {
                    if text.is_empty() {
                        // See Literal case
                        return MatchResult::AbortAll;
                    }

                    if text[0] == b'/' && self.flags.intersects(PatternFlag::PATH_NAME) {
                        return MatchResult::NoMatch;
                    }

                    text = &text[1..];
                }
                Component::Class(class) => {
                    if text.is_empty() {
                        // See Literal case
                        return MatchResult::AbortAll;
                    }

                    let mut ch = text[0];
                    if self.flags.intersects(PatternFlag::IGNORE_CASE) {
                        ch = ch.to_ascii_lowercase();
                    }
                    if !class.matches(ch) {
                        return MatchResult::NoMatch;
                    }

                    text = &text[1..];
                }
                Component::Star if self.flags.intersects(PatternFlag::PATH_NAME) => {
                    // FIXME: Optimization: Instead of .contains, fast-skip to its index, like git
                    // does.
                    if (ci + 1) == components.len() && !text.contains(&b'/') {
                        return MatchResult::Match;
                    }

                    loop {
                        if text.is_empty() {
                            // We still abort all here, but git has some optimizations we could
                            // do instead before reaching this.
                            return MatchResult::AbortAll;
                        }

                        // FIXME: Optimization: Add the "try to advance faster" optimization from
                        // git here.

                        match self.do_matches(components, ci + 1, text) {
                            MatchResult::NoMatch => {
                                if text[0] == b'/' {
                                    return MatchResult::AbortToStarStar;
                                }
                            }
                            other => return other,
                        }

                        text = &text[1..];
                    }
                }
                Component::Star | Component::StarStar => {
                    if (ci + 1) == components.len() {
                        return MatchResult::Match;
                    }

                    if let Component::StarStar = components[ci] {
                        if ci > 0
                            && components[ci - 1].ends_with_slash()
                            && ((ci + 1) == components.len()
                                || components[ci + 1].starts_with_slash())
                        {
                            // Assuming we matched `foo/` and are at `/` `**` `/`, see if we an let
                            // it match nothing, so that `foo/` `**` `/bar` can match `foo/bar`.
                            //
                            // Under the condition that the previous component ended with a slash
                            // (`components[ci - 1].ends_with_slash()`) we can safely move back by
                            // a byte in `text`.
                            let text = unsafe {
                                std::slice::from_raw_parts(text.as_ptr().offset(-1), text.len() + 1)
                            };
                            #[allow(clippy::single_match)]
                            match self.do_matches(components, ci + 1, text) {
                                MatchResult::Match => return MatchResult::Match,
                                _ => (), // or just continue regularly
                            }
                        }
                    }

                    loop {
                        if text.is_empty() {
                            // See Literal case
                            return MatchResult::AbortAll;
                        }

                        match self.do_matches(components, ci + 1, text) {
                            MatchResult::NoMatch => (),
                            MatchResult::AbortToStarStar => (), // continue from here
                            other => return other,
                        }

                        text = &text[1..];
                    }
                }
            }
            ci += 1;
        }

        if text.is_empty() {
            MatchResult::Match
        } else {
            MatchResult::NoMatch
        }
    }
}

fn starts_with(text: &[u8], with: &[u8], flags: PatternFlag) -> bool {
    if flags.intersects(PatternFlag::IGNORE_CASE) {
        starts_with_caseless(text, with)
    } else {
        text.starts_with(with)
    }
}

fn starts_with_caseless(text: &[u8], with: &[u8]) -> bool {
    if text.len() < with.len() {
        return false;
    }

    for i in 0..with.len() {
        if text[i].to_ascii_lowercase() != with[i].to_ascii_lowercase() {
            return false;
        }
    }

    true
}

#[test]
fn test() {
    let pattern = Pattern::new("/hey/*/you", PatternFlag::PATH_NAME).unwrap();
    assert!(pattern.matches("/hey/asdf/you"));
    assert!(!pattern.matches("/hey/asdf/more/you"));
    assert!(!pattern.matches("/heyasdf/you"));
    assert!(!pattern.matches("/heyasdfyou"));
    assert!(!pattern.matches("/hey/asdfyou"));
    assert!(!pattern.matches("/hey/you"));
    assert!(pattern.matches("/hey//you"));

    let pattern = Pattern::new("/hey/*/you", PatternFlag::empty()).unwrap();
    assert!(pattern.matches("/hey/asdf/you"));
    assert!(pattern.matches("/hey/asdf/more/you")); // different to PATH_NAME
    assert!(!pattern.matches("/heyasdf/you"));
    assert!(!pattern.matches("/heyasdfyou"));
    assert!(!pattern.matches("/hey/asdfyou"));
    assert!(!pattern.matches("/hey/you"));
    assert!(pattern.matches("/hey//you"));

    let pattern = Pattern::new("/hey/**/you", PatternFlag::PATH_NAME).unwrap();
    assert!(pattern.matches("/hey/asdf/you"));
    assert!(pattern.matches("/hey/asdf/more/you"));
    assert!(!pattern.matches("/heyasdf/you"));
    assert!(!pattern.matches("/heyasdfyou"));
    assert!(!pattern.matches("/hey/asdfyou"));
    assert!(pattern.matches("/hey/you"));
    assert!(pattern.matches("/hey//you"));

    let pattern = Pattern::new("/he[yx]/**/you", PatternFlag::PATH_NAME).unwrap();
    assert!(pattern.matches("/hey/asdf/you"));
    assert!(pattern.matches("/hey/asdf/more/you"));
    assert!(!pattern.matches("/heyasdf/you"));
    assert!(!pattern.matches("/heyasdfyou"));
    assert!(!pattern.matches("/hey/asdfyou"));
    assert!(pattern.matches("/hey/you"));
    assert!(pattern.matches("/hey//you"));

    assert!(pattern.matches("/hex/asdf/you"));
    assert!(pattern.matches("/hex/asdf/more/you"));
    assert!(!pattern.matches("/hexasdf/you"));
    assert!(!pattern.matches("/hexasdfyou"));
    assert!(!pattern.matches("/hex/asdfyou"));
    assert!(pattern.matches("/hex/you"));
    assert!(pattern.matches("/hex//you"));

    assert!(!pattern.matches("/hez/asdf/you"));
    assert!(!pattern.matches("/hez/asdf/more/you"));
    assert!(!pattern.matches("/hezasdf/you"));
    assert!(!pattern.matches("/hezasdfyou"));
    assert!(!pattern.matches("/hez/asdfyou"));
    assert!(!pattern.matches("/hez/you"));
    assert!(!pattern.matches("/hez//you"));

    let pattern = Pattern::new("/he[^yx]/**/you", PatternFlag::PATH_NAME).unwrap();
    assert!(!pattern.matches("/hey/asdf/you"));
    assert!(!pattern.matches("/hey/asdf/more/you"));
    assert!(!pattern.matches("/heyasdf/you"));
    assert!(!pattern.matches("/heyasdfyou"));
    assert!(!pattern.matches("/hey/asdfyou"));
    assert!(!pattern.matches("/hey/you"));
    assert!(!pattern.matches("/hey//you"));

    assert!(!pattern.matches("/hex/asdf/you"));
    assert!(!pattern.matches("/hex/asdf/more/you"));
    assert!(!pattern.matches("/hexasdf/you"));
    assert!(!pattern.matches("/hexasdfyou"));
    assert!(!pattern.matches("/hex/asdfyou"));
    assert!(!pattern.matches("/hex/you"));
    assert!(!pattern.matches("/hex//you"));

    assert!(pattern.matches("/hez/asdf/you"));
    assert!(pattern.matches("/hez/asdf/more/you"));
    assert!(!pattern.matches("/hezasdf/you"));
    assert!(!pattern.matches("/hezasdfyou"));
    assert!(!pattern.matches("/hez/asdfyou"));
    assert!(pattern.matches("/hez/you"));
    assert!(pattern.matches("/hez//you"));

    let wrong = b"/hez/";
    for i in 0..wrong.len() {
        assert!(!pattern.matches(&wrong[..i]));
    }

    let pattern = Pattern::new("/tes[a-t]", PatternFlag::PATH_NAME).unwrap();
    assert!(!pattern.matches("/testoolong"));
    assert!(!pattern.matches("/tes"));
    assert!(!pattern.matches("/t"));
    assert!(!pattern.matches("/"));
    assert!(!pattern.matches(""));
    assert!(pattern.matches("/tesa"));
    assert!(pattern.matches("/test"));
    assert!(!pattern.matches("/tesu"));

    let pattern_path = Pattern::new("/tes[a-t]/a?a", PatternFlag::PATH_NAME).unwrap();
    let pattern_nopath = Pattern::new("/tes[a-t]/a?a", PatternFlag::empty()).unwrap();
    assert!(!pattern_path.matches("/tesu"));
    assert!(!pattern_nopath.matches("/tesu"));
    assert!(!pattern_path.matches("/tesu/aaa"));
    assert!(!pattern_nopath.matches("/tesu/aaa"));
    assert!(!pattern_path.matches("/tesu/xax"));
    assert!(!pattern_nopath.matches("/tesu/xax"));
    assert!(!pattern_path.matches("/test/xax"));
    assert!(!pattern_nopath.matches("/test/xax"));
    assert!(!pattern_path.matches("/test/a"));
    assert!(!pattern_nopath.matches("/test/a"));
    assert!(!pattern_path.matches("/test/ab"));
    assert!(!pattern_nopath.matches("/test/ab"));
    assert!(pattern_path.matches("/test/aba"));
    assert!(pattern_nopath.matches("/test/aba"));
    assert!(pattern_path.matches("/test/aaa"));
    assert!(pattern_nopath.matches("/test/aaa"));
    assert!(pattern_path.matches("/test/aba"));
    assert!(pattern_nopath.matches("/test/aba"));
    // the difference is here:
    assert!(!pattern_path.matches("/test/a/a"));
    assert!(pattern_nopath.matches("/test/a/a"));

    let pattern = Pattern::new("a*b*c", PatternFlag::PATH_NAME).unwrap();
    assert!(pattern.matches("axxbxxc"));
    assert!(!pattern.matches("axxbxxcxx"));
    assert!(pattern.matches("axxbxxbxxc"));
    assert!(!pattern.matches("axxbxxbxxcxx"));
    assert!(pattern.matches("axxbxxbxxcxxc"));
    assert!(!pattern.matches("axxbxxbxxcxxcxx"));

    let pattern = Pattern::new("a*b*c*", PatternFlag::PATH_NAME).unwrap();
    assert!(pattern.matches("axxbxxc"));
    assert!(pattern.matches("axxbxxcxx"));
    assert!(pattern.matches("axxbxxbxxc"));
    assert!(pattern.matches("axxbxxbxxcxx"));
    assert!(pattern.matches("axxbxxbxxcxxc"));
    assert!(pattern.matches("axxbxxbxxcxxcxx"));

    let pattern = Pattern::new(
        "aB[c-fX-Z][[:upper:]][[:lower:]][[:digit:]k]",
        PatternFlag::PATH_NAME | PatternFlag::IGNORE_CASE,
    )
    .unwrap();
    eprintln!("{:#?}", pattern);
    assert!(pattern.matches("aBcUl3"));
    assert!(pattern.matches("AbCuL9"));
    assert!(!pattern.matches("aBgUl3"));
    assert!(!pattern.matches("aBgUl3"));
    assert!(!pattern.matches("aBcUlx"));
    assert!(pattern.matches("abculk"));
    assert!(pattern.matches("abxulk"));
    assert!(!pattern.matches("abxul"));

    let pattern = Pattern::new("a/b**/c", PatternFlag::PATH_NAME).unwrap();
    assert!(pattern.matches("a/bxx/c"));
    assert!(!pattern.matches("a/bxx/yy/c"));
}
