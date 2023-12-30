use core::fmt;
use std::cell::{Cell, RefCell};
use std::collections::HashSet;
use std::hash::{Hash, Hasher};

use thiserror::Error;
use winsafe::{co, DISPLAY_DEVICE, EnumDisplayDevices};
use serde::{Deserialize, Serialize};
use winsafe::co::EDD;
use winsafe::prelude::NativeBitflag;


use crate::{DisplayPropertiesError, DisplayState, Resolution, FixedOutput, Orientation};

// This should be a generator instead, but I like the set based APIs. Since DisplayState are truly unique per entry (key'd by devicekey),
// you can perform all kind of set operations on them.
/// Returns a list of all displays.
pub fn get_all_display_set() -> HashSet<DisplayState> {
    let all_displays: HashSet<DisplayState> = EnumDisplayDevices(None, None)
        .filter(|result| result.is_ok())
        .map(|display| DisplayState::new(display.unwrap()))
        .filter(|display_state_option| display_state_option.is_some())
        .map(|display_state| display_state.unwrap())
        .collect();

    return all_displays;
}

// /// Refreshes the screen to apply the changes
// pub fn commit_display_changes() -> DisplayResult {
//     let result = winsafe::ChangeDisplaySettingsEx(None, None, winsafe::co::CDS::DYNAMICALLY);
//     match result {
//         Ok(_) => Ok(()),
//         Err(err) => Err(DisplayError::FailedToCommit(err)),
//     }
// }
// 

pub struct DisplaySetChangeContext<'a> {
    // The displays we're about to be changing.
    changable_display_states: &'a HashSet<DisplayState>,
    
    // Duplicate of the initial starting state, used for diffing.
    original_display_states: HashSet<DisplayState>
}

impl DisplaySetChangeContext<'_> {
    pub fn new(changing_display_states: &HashSet<DisplayState>) -> DisplaySetChangeContext {
        DisplaySetChangeContext {
            changable_display_states: changing_display_states,
            original_display_states: changing_display_states.to_owned(),
        }
    }
    
    pub fn commit_changes(&self) {
        
    } 
}