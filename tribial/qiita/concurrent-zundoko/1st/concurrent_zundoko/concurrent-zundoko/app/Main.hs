{-# LANGUAGE LambdaCase, TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent.STM

import Tips
import RealtimeQueueStm
import ZunDoko

data ZunDoko = Zun | Doko | Kiyoshi deriving (Show, Eq)

instance ToEndable ZunDoko where
	type PreEndable ZunDoko = ZunDoko
	endValue = Kiyoshi

say :: ZunDoko -> String
say = \case Zun -> "ズン"; Doko -> "ドコ"; Kiyoshi -> "キ・ヨ・シ!"

main :: IO ()
-- main = zundoko [Zun, Doko] [Zun, Zun, Zun, Zun, Doko] >>= \q -> loopIf
--	$ (<$) <$> (/= endValue) <*> putStrLn . say =<< atomically (dequeue q)
main = do
	q <- zundoko [Zun, Doko] [Zun, Zun, Zun, Zun, Doko]
	loopIf $ do
		z <- atomically $ dequeue q
		putStrLn $ say z
		pure $ z /= endValue
