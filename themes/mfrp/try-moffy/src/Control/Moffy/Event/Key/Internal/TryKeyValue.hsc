{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Key.Internal.TryKeyValue (
	-- * Void symbol
	pattern XkVoidsymbol,
	-- * TTY function keys
	pattern XkBackspace, pattern XkTab,
	pattern XkLinefeed, pattern XkClear, pattern XkReturn,
	pattern XkPause, pattern XkScrollLock, pattern XkSysReq,
	pattern XkEscape, pattern XkDelete ) where

import Control.Moffy.Event.Key.Internal

#define XK_MISCELLANY

#include "X11/keysymdef.h"

#enum Key, Key, XK_VoidSymbol

#enum Key, Key, \
	XK_BackSpace, XK_Tab, XK_Linefeed, XK_Clear, XK_Return, XK_Pause, \
	XK_Scroll_Lock, XK_Sys_Req, XK_Escape, XK_Delete

#enum Key, Key, \
	XK_Multi_key, XK_Codeinput, XK_SingleCandidate, XK_MultipleCandidate, \
	XK_PreviousCandidate

#enum Key, Key, \
	XK_Kanji, XK_Muhenkan, XK_Henkan_Mode, XK_Romaji, XK_Hiragana, \
	XK_Katakana, XK_Hiragana_Katakana, XK_Zenkaku, XK_Hankaku, \
	XK_Zenkaku_Hankaku, XK_Touroku, XK_Massyo, XK_Kana_Lock, \
	XK_Kana_Shift, XK_Eisu_Shift, XK_Eisu_toggle, XK_Kanji_Bangou, \
	XK_Zen_Koho, XK_Mae_Koho

concat <$> foo `mapM` [
	'xkVoidsymbol,
	'xkBackspace, 'xkTab, 'xkLinefeed, 'xkClear, 'xkReturn, 'xkPause,
	'xkScrollLock, 'xkSysReq, 'xkEscape, 'xkDelete ]
