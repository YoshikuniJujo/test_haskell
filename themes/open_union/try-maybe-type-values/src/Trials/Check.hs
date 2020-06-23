{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Check where

import Data.Type.Set
import Data.OneOrMore

import MonadicFrp
import MonadicFrp.Run

data Foo = FooReq deriving (Show, Eq, Ord)
numbered 9 [t| Foo |]
instance Request Foo where data Occurred Foo = OccFoo deriving Show

foo :: React (Singleton Foo) ()
foo = await FooReq \OccFoo -> ()

data Bar = BarReq deriving (Show, Eq, Ord)
numbered 9 [t| Bar |]
instance Request Bar where data Occurred Bar = OccBar deriving Show

bar :: React (Singleton Bar) ()
bar = await BarReq \OccBar -> ()

data Baz = BazReq deriving (Show, Eq, Ord)
numbered 9 [t| Baz |]
instance Request Baz where data Occurred Baz = OccBaz deriving Show

baz :: React (Singleton Baz) ()
baz = await BazReq \OccBaz -> ()

foobar :: React (Foo :- Bar :- 'Nil) ()
foobar = do
	adjust foo
	adjust bar

handle :: EvReqs (Foo :- Bar :- Baz :- 'Nil) -> IO (EvOccs (Foo :- Bar :- Baz :- 'Nil))
handle reqs = do
	case prj reqs of
		Just FooReq -> putStrLn "FooReq"
		Nothing -> putStrLn "No FooReq"
	case prj reqs of
		Just BarReq -> putStrLn "BarReq"
		Nothing -> putStrLn "No BarReq"
	pure $ expand (OccFoo >- singleton OccBar :: EvOccs (Foo :- Bar :- 'Nil))

tryFoobar :: IO ()
tryFoobar = interpretReact handle (adjust foobar)
