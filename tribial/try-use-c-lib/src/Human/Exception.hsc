{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Human.Exception where

import Foreign.C.Enum
import Data.Word

#include <human.h>

enum "DrawHumanResult" ''#{type HmDrawHumanResult} [''Show, ''Read] [
	("DrawHumanResultSuccess", #{const HM_DRAW_HUMAN_SUCCESS})
	]
