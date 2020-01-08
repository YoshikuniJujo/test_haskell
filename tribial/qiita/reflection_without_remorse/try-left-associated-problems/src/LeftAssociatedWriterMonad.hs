{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LeftAssociatedWriterMonad where

import Control.Monad

data Writer a = Writer a String deriving Show

getLog :: Writer a -> String
getLog (Writer _ w) = w

bind :: Writer a -> (a -> Writer b) -> Writer b
Writer x w `bind` f = let Writer y w' = f x in Writer y $ w ++. w'

instance Functor Writer where f `fmap` Writer x w = Writer (f x) w

instance Applicative Writer where
	pure = (`Writer` "")
	mf <*> mx = mf `bind` \f -> mx `bind` \x -> pure $ f x

instance Monad Writer where (>>=) = bind

sampleWriter :: Writer ()
sampleWriter = Writer () "hello"

sampleFun :: () -> Writer ()
sampleFun = const sampleWriter

sampleL, sampleR :: () -> Writer ()
sampleL = {-# SCC "LeftAssociatedHellos" #-}
	foldl (>=>) pure $ replicate 8000 sampleFun

sampleR = {-# SCC "RightAssoociatedHellos" #-}
	foldr (>=>) pure $ replicate 8000 sampleFun

(++.) :: [a] -> [a] -> [a]
[] ++. ys = ys
(x : xs) ++. ys = x : (xs ++. ys)
