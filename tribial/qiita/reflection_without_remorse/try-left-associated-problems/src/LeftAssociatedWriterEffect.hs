{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LeftAssociatedWriterEffect where

import Control.Monad
import Data.String

import Eff
import Writer

newtype Str = Str String deriving Show

last' :: Str -> Char
last' (Str s) = last s

(.:.) :: Char -> Str -> Str
c .:. Str cs = Str $ c : cs

(++.) :: Str -> Str -> Str
Str "" ++. s = {-# SCC "AppendStrEmpty" #-} s
Str (c : cs) ++. Str ds = {-# SCC "AppendStrNotEmpty" #-} c .:. (Str cs ++. Str ds)

instance Semigroup Str where (<>) = {-# SCC "AppendStr" #-} (++.)
instance Monoid Str where mempty = Str ""
instance IsString Str where fromString = Str

sampleWriter :: Eff '[Writer Str] ()
sampleWriter = tell ("hello" :: Str)

sampleL, sampleR :: () -> Eff '[Writer Str] ()
sampleL = {-# SCC "LeftAssociatedHellos" #-}
	foldl (>=>) pure . replicate 8000 $ const sampleWriter
sampleR = {-# SCC "RightAssociatedHellos" #-}
	foldr (>=>) pure . replicate 8000 $ const sampleWriter
