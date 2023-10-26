use serde::*;
use std::borrow::Cow;
use std::path::Path;

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct CowStr<'a>(#[serde(borrow)] pub Cow<'a, str>);

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct CowPath<'a>(#[serde(borrow)] pub Cow<'a, Path>);
