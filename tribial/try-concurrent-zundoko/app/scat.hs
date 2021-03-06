{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Tips (loopIf)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.RTQueue (dequeue)
import Data.Maybe (isJust)

import ZunDoko (zundoko)

data Scat = Be | Bop | A | Bodda deriving (Show, Eq)

main :: IO ()
main = zundoko [Be, Bop, A, Bodda] [Bop, A, Bodda, Bop] >>= \q ->
	loopIf $ (<$) <$> isJust <*> maybe (return ()) print
		=<< atomically (dequeue q)
