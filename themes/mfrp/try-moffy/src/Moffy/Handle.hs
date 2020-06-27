{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.Handle where

import Data.OneOrMore hiding (expand, collapse)

import qualified Data.OneOrMore as OOM

collapse hdl = maybe (pure Nothing) hdl . OOM.collapse

expand hdl = ((OOM.expand <$>) <$>) . collapse hdl

-- infixr 5 `before`

before hdl1 hdl2 rqs = maybe (expand hdl2 rqs) (pure . Just) =<< expand hdl1 rqs
