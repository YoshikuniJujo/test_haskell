{-# LANGUAGE LambdaCase, TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Tips (loopIf)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.RTQueue (dequeue)

import ZunDoko (ToEndable(..), zundoko)

instance ToEndable ZunDoko where
	type PreEndable ZunDoko = ZunDoko
	endValue = Kiyoshi

data ZunDoko = Zun | Doko | Kiyoshi deriving (Show, Eq)

say :: ZunDoko -> String
say = \case Zun -> "ズン"; Doko -> "ドコ"; Kiyoshi -> "キ・ヨ・シ!"

main :: IO ()
main = zundoko [Zun, Doko] [Zun, Zun, Zun, Zun, Doko] >>= \q -> loopIf
	$ (<$) <$> (/= endValue) <*> putStrLn . say =<< atomically (dequeue q)
