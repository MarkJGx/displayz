﻿use color_eyre::owo_colors::OwoColorize;
use serde_json::ser::State::First;
use displayz::{get_all_display_set, DisplaySetChangeContext, RefreshRate, Orientation, Resolution};

fn main() {
    {
        let mut all_displays = get_all_display_set();

        let mut display_change_context = DisplaySetChangeContext::new(&mut all_displays);


        let mut first_display_state = display_change_context.changeable_display_states.iter_mut().next().unwrap();
        first_display_state.refresh_rate = RefreshRate(144);
        first_display_state.resolution = Resolution::new(3840, 2160);
        first_display_state.is_enabled = true;

        display_change_context.commit_changes();
    }
}