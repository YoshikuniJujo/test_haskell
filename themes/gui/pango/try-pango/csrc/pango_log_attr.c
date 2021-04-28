#include <pango/pango.h>
#include "pango_log_attr.h"

void
pango_log_attr_to_struct(PangoLogAttr *src, PangoLogAttrStr *dst)
{
	dst->is_line_break = src->is_line_break;
	dst->is_mandatory_break = src->is_mandatory_break;
	dst->is_char_break = src->is_char_break;
	dst->is_white = src->is_white;
	dst->is_cursor_position = src->is_cursor_position;
	dst->is_word_start = src->is_word_start;
	dst->is_word_end = src->is_word_end;
	dst->is_sentence_boundary = src->is_sentence_boundary;
	dst->is_sentence_start = src->is_sentence_end;
	dst->is_sentence_end = src->is_sentence_end;
	dst->backspace_deletes_character = src->backspace_deletes_character;
	dst->is_expandable_space = src->is_expandable_space;
	dst->is_word_boundary = src->is_word_boundary;
}
