//! A library to interact with the Windows API for display settings.
//!
//! This library provides an abstraction around some `winuser.h` calls relevant for modifying display settings.

mod display;
mod types;

pub use display::*;
pub use types::*;
