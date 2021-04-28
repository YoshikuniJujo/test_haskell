#ifndef _PANGO_LOG_ATTR_H
#define _PANGO_LOG_ATTR_H

#include <pango/pango.h>

typedef struct {
	guint is_line_break;
	guint is_mandatory_break;
	guint is_char_break;
	guint is_white;
	guint is_cursor_position;
	guint is_word_start;
	guint is_word_end;
	guint is_sentence_boundary;
	guint is_sentence_start;
	guint is_sentence_end;
	guint backspace_deletes_character;
	guint is_expandable_space;
	guint is_word_boundary;
	} PangoLogAttrStr;

#endif

