#ifndef _GDKHS_H
#define _GDKHS_H

#define peek_gdk_event_key_is_modifier(p) (p -> is_modifier)
#define poke_gdk_event_key_is_modifier(p, b) (p -> is_modifier = b)

#endif
