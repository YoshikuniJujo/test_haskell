{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Tips (loopIf)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.RTQueue (dequeue)

import Zundoko (zundoko)

data ZunDoko = Zun | Doko | Kiyoshi deriving (Show, Eq)

say :: ZunDoko -> String
say = \case Zun -> "ズン"; Doko -> "ドコ"; Kiyoshi -> "キ・ヨ・シ!"

main :: IO ()
main = zundoko [Zun, Doko] [Zun, Zun, Zun, Zun, Doko] Kiyoshi >>= \q -> loopIf
	$ (<$) <$> (/= Kiyoshi) <*> putStrLn . say =<< atomically (dequeue q)
