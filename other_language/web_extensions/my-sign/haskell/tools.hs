{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Control.Arrow
import Data.Bool
import Data.Char

camelToSnake :: String -> String
camelToSnake = go2 True
	where
	go "" = ""
	go (c : cs)
		| isUpper c = '_' : toLower c : go2 False cs
		| otherwise = c : go cs
	go2 _ "" = ""
	go2 _ [c] = [toLower c]
	go2 h (c : cs@(d : _))
		| isUpper c && isUpper d = toLower c : go2 False cs
		| isUpper c && isLower d =
			bool ('_' :) id h $ toLower c : go cs
		| otherwise = toLower c : go cs
