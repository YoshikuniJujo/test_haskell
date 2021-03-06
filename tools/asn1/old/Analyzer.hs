{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Analyzer (Analyzer, spot, ret, bind, build, next, failure, listAll) where

import Data.Word8

import qualified Data.ByteString as BS

infixr 8 `build`
infixr 7 `next`
infixr 7 `bind`

type Analyzer a = BS.ByteString -> Maybe (a, BS.ByteString)

spot :: (Word8 -> Bool) -> Analyzer Word8
spot p bs
	| Just (h, t) <- BS.uncons bs = if p h then Just (h, t) else Nothing
	| otherwise = Nothing

eof :: Analyzer Bool
eof "" = Just (True, "")
eof bs = Just (False, bs)

build :: (a -> b) -> Analyzer a -> Analyzer b
f `build` a = a `bind` ret . f

next :: Analyzer a -> Analyzer b -> Analyzer (a, b)
a1 `next` a2 = a1 `bind` \x1 -> (x1 ,) `build` a2

bind :: Analyzer a -> (a -> Analyzer b) -> Analyzer b
(a1 `bind` a2) bs =  case a1 bs of
	Just (x, r) -> a2 x r
	_ -> Nothing

ret :: a -> Analyzer a
ret = (Just .) . (,)

failure :: Analyzer a
failure = const Nothing

listAll :: Analyzer a -> Analyzer [a]
listAll a = eof `bind` \e -> if e then ret [] else
	a `bind` \x -> (x :) `build` listAll a
