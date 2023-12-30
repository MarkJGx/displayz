use displayz::{get_all_display_set, DisplaySetChangeContext};

fn main() {
    let all_displays = get_all_display_set();
    
    let display_change_context = DisplaySetChangeContext::new(&all_displays);
    display_change_context.commit_changes();
}