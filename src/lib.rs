//! Include/Exclude file pattern matching.
//!
//! This implements a glob `Pattern` similar to `git`'s matching done in include/exclude files, and
//! some helper methods to figure out whether a file should be included given its designated path
//! and a list of include/exclude patterns.
//!
//! Here's a rather long matching example:
//!
//! ```
//! # use pathpatterns::*;
//! # fn test() -> Result<(), Box<dyn std::error::Error + Send + Sync + 'static>> {
//! let file_list: &'static [&'static [u8]] = &[
//!     b"/things",
//!     b"/things/file1.dat",
//!     b"/things/file2.dat",
//!     b"/things/shop",
//!     b"/things/shop/info.txt",
//!     b"/things/shop/apples",
//!     b"/things/shop/apples/gala.txt",
//!     b"/things/shop/apples/golden-delicious.txt",
//!     b"/things/shop/bananas",
//!     b"/things/shop/bananas/straight.txt",
//!     b"/things/shop/bananas/curved.txt",
//!     b"/things/shop/bananas/curved.bak",
//!     b"/things/shop/bananas/other.txt",
//! ];
//!
//! let mut list = vec![
//!     MatchEntry::include(Pattern::path("shop")?),
//!     MatchEntry::exclude(Pattern::path("bananas")?),
//!     MatchEntry::include(Pattern::path("bananas/curved.*")?),
//! ];
//!
//! assert_eq!(list.matches("/things", None), None);
//! assert_eq!(list.matches("/things/shop", None), Some(MatchType::Include));
//! assert_eq!(list.matches("/things/shop/bananas", None), Some(MatchType::Exclude));
//! assert_eq!(list.matches("/things/shop/bananas/curved.txt", None), Some(MatchType::Include));
//! assert_eq!(list.matches("/things/shop/bananas/curved.bak", None), Some(MatchType::Include));
//!
//! // this will exclude the curved.bak file
//! list.push(MatchEntry::exclude(Pattern::path("curved.bak")?));
//! assert_eq!(list.matches("/things/shop/bananas/curved.bak", None), Some(MatchType::Exclude));
//! list.pop();
//! # assert_eq!(list.matches("/things/shop/bananas/curved.bak", None), Some(MatchType::Include));
//!
//! // but this will not:
//! list.push(
//!     MatchEntry::new(Pattern::path("curved.bak")?, MatchType::Exclude)
//!         .flags(MatchFlag::ANCHORED)
//! );
//! // or: list.push
//! assert_eq!(list.matches("/things/shop/bananas/curved.bak", None), Some(MatchType::Include));
//! list.pop();
//!
//! // let's check some patterns, anything starting with a 'c', 'f' or 's':
//! let mut list = vec![MatchEntry::include(Pattern::path("[cfs]*")?)];
//! assert_eq!(list.matches("/things", None), None);
//! assert_eq!(list.matches("/things/file1.dat", None), Some(MatchType::Include));
//! assert_eq!(list.matches("/things/file2.dat", None), Some(MatchType::Include));
//! assert_eq!(list.matches("/things/shop", None), Some(MatchType::Include));
//! assert_eq!(list.matches("/things/shop/info.txt", None), None);
//! assert_eq!(list.matches("/things/shop/apples", None), None);
//! assert_eq!(list.matches("/things/shop/apples/gala.txt", None), None);
//! assert_eq!(list.matches("/things/shop/apples/golden-delicious.txt", None), None);
//! assert_eq!(list.matches("/things/shop/bananas", None), None);
//! assert_eq!(list.matches("/things/shop/bananas/straight.txt", None), Some(MatchType::Include));
//! assert_eq!(list.matches("/things/shop/bananas/curved.txt", None), Some(MatchType::Include));
//! assert_eq!(list.matches("/shop/bananas/curved.bak", None), Some(MatchType::Include));
//! assert_eq!(list.matches("/things/shop/bananas/other.txt", None), None);
//!
//! // If we add `**` we end up including the entire `shop/` subtree:
//! list.push(MatchEntry::include(Pattern::path("[cfs]*/**")?));
//! assert_eq!(list.matches("/things", None), None);
//! assert_eq!(list.matches("/things/file1.dat", None), Some(MatchType::Include));
//! assert_eq!(list.matches("/things/file2.dat", None), Some(MatchType::Include));
//! assert_eq!(list.matches("/things/shop", None), Some(MatchType::Include));
//! assert_eq!(list.matches("/things/shop/info.txt", None), Some(MatchType::Include));
//! assert_eq!(list.matches("/things/shop/apples", None), Some(MatchType::Include));
//! assert_eq!(list.matches("/things/shop/apples/gala.txt", None), Some(MatchType::Include));
//! assert_eq!(list.matches("/shop/apples/golden-delicious.txt", None), Some(MatchType::Include));
//! assert_eq!(list.matches("/things/shop/bananas", None), Some(MatchType::Include));
//! assert_eq!(list.matches("/things/shop/bananas/straight.txt", None), Some(MatchType::Include));
//! assert_eq!(list.matches("/things/shop/bananas/curved.txt", None), Some(MatchType::Include));
//! assert_eq!(list.matches("/shop/bananas/curved.bak", None), Some(MatchType::Include));
//! assert_eq!(list.matches("/shop/bananas/other.txt", None), Some(MatchType::Include));
//! # Ok(())
//! # }
//! # test().unwrap()
//! ```

mod match_list;
mod pattern;

#[doc(inline)]
pub use match_list::{MatchEntry, MatchFlag, MatchList, MatchPattern, MatchType};

#[doc(inline)]
pub use pattern::{ParseError, Pattern, PatternFlag};
