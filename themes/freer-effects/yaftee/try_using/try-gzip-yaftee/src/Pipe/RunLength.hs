{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.RunLength where

import Data.Word
import Data.ByteString qualified as BS

data R = Literal Word8 | LiteralBS BS.ByteString
	| LenDist Length Dist | EndOfInput deriving Show

type Length = Int
type Dist = Int
