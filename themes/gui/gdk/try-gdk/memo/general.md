general memo
============

Function
--------

### not deprecated

* `gdk_init`
* `gdk_init_check`
* `gdk_parse_args`
* `gdk_get_display_arg_name`
* `gdk_notify_startup_complete`
* `gdk_notify_startup_complete_with_id`
* `gdk_set_allowed_backends`
* `gdk_get_program_class`
* `gdk_set_program_class`

### deprecated

* `gdk_get_display`
* `gdk_flush`
* `gdk_screen_width`
* `gdk_screen_height`
* `gdk_screen_width_mm`
* `gdk_screen_height_mm`
* `gdk_pointer_grab`
* `gdk_pointer_ungrab`
* `gdk_pointer_is_grabbed`
* `gdk_set_double_click_time`
* `gdk_keyboard_grab`
* `gdk_keyboard_ungrab`
* `gdk_beep`
* `gdk_error_trap_push`
* `gdk_error_trap_pop`
* `gdk_error_trap_pop_ignored`

### implement

* [x] `gdk_init_check`
* [x] `gdk_get_display_arg_name`
* [ ] `gdk_notify_startup_complete`
* [x] `gdk_set_allowed_backends`
* [x] `gdk_get_program_class`
* [x] `gdk_set_program_class`

### not implement yet
* `gdk_parse_args`
* `gdk_notify_startup_complete_with_id`

### won't implement

* `gdk_init`
