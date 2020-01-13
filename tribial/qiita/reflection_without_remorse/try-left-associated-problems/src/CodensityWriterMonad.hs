{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CodensityWriterMonad (sampleLWriter, sampleRWriter, getLog) where

import Prelude hiding (abs)
import Control.Monad

import CodensityMonad
import LeftAssociatedWriterMonad (Writer, sampleFun, getLog)

sampleL, sampleR :: () -> CodensityT Writer ()
sampleL = foldl (>=>) pure $ replicate 8000 (rep . sampleFun)
sampleR = foldr (>=>) pure $ replicate 8000 (rep . sampleFun)

sampleLWriter, sampleRWriter :: () -> Writer ()
sampleLWriter = {-# SCC "LeftAssociatedHellos" #-} abs . sampleL
sampleRWriter = {-# SCC "RightAssociatedHellos" #-} abs . sampleR

