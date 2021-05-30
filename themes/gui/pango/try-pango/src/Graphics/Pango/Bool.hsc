{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Bool where

import Data.Int

#include <pango/pango.h>

gbooleanToBool :: #{type gboolean} -> Bool
gbooleanToBool #{const FALSE} = False
gbooleanToBool #{const TRUE} = True
gbooleanToBool _ = error "bad gboolean"

boolToGboolean :: Bool -> #{type gboolean}
boolToGboolean False = #{const FALSE}
boolToGboolean True = #{const TRUE}
