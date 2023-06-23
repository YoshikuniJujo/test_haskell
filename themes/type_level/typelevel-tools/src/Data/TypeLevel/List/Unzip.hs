{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel.List.Unzip where

import Prelude hiding (unzip)

import Data.TypeLevel.List.TH
import Data.TypeLevel.List.Push

unzip `mapM` [2 .. 62]
