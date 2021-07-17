#ifndef _GDKHS_H
#define _GDKHS_H

#define peek_gdk_event_key_is_modifier(p) (p -> is_modifier)
#define poke_gdk_event_key_is_modifier(p, b) (p -> is_modifier = b)

#define peek_gdk_event_scroll_is_stop(p) (p -> is_stop)
#define poke_gdk_event_scroll_is_stop(p, b) (p -> is_stop = b)

#endif
