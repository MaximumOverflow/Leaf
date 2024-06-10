use std::collections::HashMap;
use std::path::PathBuf;
use serde::{Deserialize, Deserializer};
use serde::de::{Error, Unexpected};

pub type Dependencies = HashMap<String, Dependency>;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Version {
	pub major: u16,
	pub minor: u16,
	pub patch: u16,
}

impl Into<leaf_reflection::Version> for Version {
	#[inline]
	fn into(self) -> leaf_reflection::Version {
		leaf_reflection::Version {
			major: self.major,
			minor: self.minor,
			patch: self.patch,
		}
	}
}

impl<'de> Deserialize<'de> for Version {
	fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		let str = String::deserialize(deserializer)?;
		let mut parts = str.split('.');
		let Some(major) = parts.next() else {
			return Err(Error::missing_field("major"));
		};
		let Some(minor) = parts.next() else {
			return Err(Error::missing_field("minor"));
		};
		let Some(patch) = parts.next() else {
			return Err(Error::missing_field("patch"));
		};
		Ok(Self {
			major: major.parse().map_err(|_| {
				Error::invalid_value(Unexpected::Other(major), &"major version (integer)")
			})?,
			minor: minor.parse().map_err(|_| {
				Error::invalid_value(Unexpected::Other(minor), &"minor version (integer)")
			})?,
			patch: patch
				.parse()
				.map_err(|_| Error::invalid_value(Unexpected::Other(patch), &"patch (integer)"))?,
		})
	}
}

#[derive(Debug, Deserialize)]
pub struct Package {
	pub name: String,
	pub version: Version,
	pub language_version: u32,
}

#[derive(Debug, Deserialize)]
pub struct Dependency {
	pub version: Version,
}

#[derive(Debug, Deserialize)]
pub struct Directories {
	#[serde(default = "src_dir_default")]
	pub src_dir: PathBuf,
	#[serde(default = "out_dir_default")]
	pub out_dir: PathBuf,
}

impl Default for Directories {
	#[inline]
	fn default() -> Self {
		Self {
			src_dir: src_dir_default(),
			out_dir: out_dir_default(),
		}
	}
}

#[derive(Debug, Deserialize)]
pub struct CompilationConfig {
	#[serde(alias = "assembly")]
	pub package: Package,
	#[serde(default = "Default::default")]
	pub dependencies: Dependencies,
	#[serde(default = "Default::default")]
	pub directories: Directories,
}

#[inline]
fn src_dir_default() -> PathBuf {
	let mut path = std::env::current_dir().unwrap();
	path.push("src");
	path
}

#[inline]
fn out_dir_default() -> PathBuf {
	let mut path = std::env::current_dir().unwrap();
	path.push("out");
	path
}
