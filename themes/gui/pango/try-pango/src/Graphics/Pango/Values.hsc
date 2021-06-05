{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Values where

#include <pango/pango.h>

pangoScale :: Num n => n
pangoScale = #const PANGO_SCALE
