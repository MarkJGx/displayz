use core::fmt;
use std::cell::{Cell, RefCell};
use std::collections::HashSet;
use std::hash::{Hash, Hasher};

use thiserror::Error;
use winsafe::{ChangeDisplaySettings, co, DISPLAY_DEVICE, EnumDisplayDevices};
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


pub struct DisplaySetChangeContext {
    // The displays we're about to be changing.
    pub changeable_display_states: Vec<DisplayState>,
    
    // Duplicate of the initial starting state, used for diffing.
    original_display_states: HashSet<DisplayState>
}

impl DisplaySetChangeContext {
    pub fn new(changing_displays: &mut HashSet<DisplayState>) -> DisplaySetChangeContext {
        let original_display_states: HashSet<DisplayState> = changing_displays.to_owned();
        let mut changeable_display_states = changing_displays.iter().cloned().collect::<Vec<DisplayState>>();
        
        DisplaySetChangeContext {
            changeable_display_states,
            original_display_states
        }
    }

    fn change_display_state(&self, new_display_state: &DisplayState, original_display_state: &DisplayState) -> Result<bool, co::DISP_CHANGE> {
        let mut is_primary_changed = false;
        let mut is_enabled_changed = false;
        
        let mut changed_devmode = winsafe::DEVMODE::default();
        let mut changed_cds_flags =
            winsafe::co::CDS::UPDATEREGISTRY | winsafe::co::CDS::NORESET | winsafe::co::CDS::GLOBAL;
        
        if new_display_state.position != original_display_state.position {
            changed_devmode.set_dmPosition(new_display_state.position.0);
            changed_devmode.dmFields |= co::DM::POSITION;
        }

        if new_display_state.resolution != original_display_state.resolution {
            changed_devmode.dmPelsWidth = new_display_state.resolution.width;
            changed_devmode.dmPelsHeight = new_display_state.resolution.height;
            changed_devmode.dmFields |= co::DM::PELSWIDTH | co::DM::PELSHEIGHT;
        }


        if new_display_state.refresh_rate != original_display_state.refresh_rate {
            changed_devmode.dmDisplayFrequency = new_display_state.refresh_rate.0;
            changed_devmode.dmFields |= co::DM::DISPLAYFREQUENCY;
        }

        if new_display_state.fixed_output != original_display_state.fixed_output {
            changed_devmode.set_dmDisplayFixedOutput(new_display_state.fixed_output.to_winsafe());
            changed_devmode.dmFields |= co::DM::DISPLAYFIXEDOUTPUT;
        }

        if new_display_state.orientation != original_display_state.orientation {
            changed_devmode.set_dmDisplayOrientation(new_display_state.orientation.to_winsafe());
            changed_devmode.dmFields |= co::DM::DISPLAYORIENTATION;
        }

        if new_display_state.is_primary != original_display_state.is_primary {
            changed_cds_flags |= co::CDS::SET_PRIMARY;
        }

        if new_display_state.is_enabled != original_display_state.is_enabled {
            is_enabled_changed = true;
        }

        // if anything has changed at all
        if changed_devmode.dmFields.raw() > 0 {
            let change_result = ChangeDisplaySettings(Some(&mut changed_devmode), changed_cds_flags);

            return match change_result {
                Ok(_) => Ok(true),
                Err(error) => Err(error)
            }
        }
        
        // Operation succeeded, but we didnt' change anything.
        return Ok(false);
    }

    pub fn commit_changes(&self) {
        let changed_set = HashSet::from_iter(self.changeable_display_states.iter().cloned());
        debug_assert!(!changed_set.symmetric_difference(&self.original_display_states).next().is_some(),
                      "changeable_display_states doesn't have the same keys as the original starting set!");


        // Make sure the primary display is always first.
        let mut sorted_states: Vec<DisplayState> = self.changeable_display_states.clone();
        sorted_states.sort_by((|a, b| b.is_primary.cmp(&a.is_primary)));
        println!("{:#?}", sorted_states);
        
        // @todo: Convert this to an error.
        assert_eq!(sorted_states.iter().filter(|display_state| display_state.is_primary).collect::<Vec<&DisplayState>>().len(), 1, "There has to be 1 and only 1 primary display. Look at the left value.");

        let changed_displays = sorted_states.iter().filter(|&state|
        {
            self.change_display_state(state, self.original_display_states.get(&state).unwrap()).is_ok()
        }).count();

        if changed_displays > 0 {
            /// Refreshes the screen to apply the changes
            let result = winsafe::ChangeDisplaySettingsEx(None, None, winsafe::co::CDS::DYNAMICALLY);
        }
        
        // match result {
        //     Ok(_) => Ok(()),
        //     Err(err) => Err(DisplayError::FailedToCommit(err)),
        // }
        return;
    }
}