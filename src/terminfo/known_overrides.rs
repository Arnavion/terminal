// TODO: Maybe these should be read from some config file, but it's hard to do that while remaining "tiny".

use super::Capability;

#[derive(Clone, Copy, Debug)]
pub struct Overrides {
	pub string_capabilities: &'static [(usize, Capability<&'static [u8]>)],
	pub extended_string_capabilities: &'static [(&'static [u8], Capability<&'static [u8]>)],
}

pub(super) const KNOWN_OVERRIDES: &[(&str, Overrides)] = &[
	("tmux-256color", Overrides {
		string_capabilities: &[
			(151, Capability::Value(b"\x1b[?7h")),
			(152, Capability::Value(b"\x1b[?7l")),
		],
		extended_string_capabilities: &[],
	}),
];
