{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.Functor.Identity
import Data.Functor.Const

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Lens s a = forall f . Functor f => (a -> f a) -> s -> f s

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
-- lens gt st f s = st s <$> f (gt s)
lens gt st f = \s -> st s <$> f (gt s)

get :: Lens s a -> s -> a
get l s = getConst $ l Const s

set :: Lens s a -> s -> a -> s
set l s a = runIdentity $ l (const $ Identity a) s

data Foo = Foo { fooA :: Char, fooB :: Bar, fooC :: Bool } deriving Show

data Bar = Bar { barX :: Double, barY :: Int, barZ :: Bool } deriving Show

b :: Lens Foo Bar
b = lens fooB \f b -> f { fooB = b }

y :: Lens Bar Int
y = lens barY \b y -> b { barY = y }

sampleFoo :: Foo
sampleFoo = Foo 'c' (Bar 123 321 False) True
