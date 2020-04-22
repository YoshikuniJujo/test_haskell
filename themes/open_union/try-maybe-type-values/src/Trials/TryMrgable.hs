{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.TryMrgable where

import Data.Type.Set
import Data.UnionSet
import Data.Bool
import System.Random

import MonadicFrp

data Result = Failure | Succeed deriving Show

result :: a -> a -> Result -> a
result f _ Failure = f
result _ s Succeed = s

data StoreRandomGen = StoreRandomGen StdGen deriving Show
numbered 4 [t| StoreRandomGen |]
instance Mrgable StoreRandomGen where sg1 `mrg` _srgg2 = sg1
instance Request StoreRandomGen where
	data Occurred StoreRandomGen = OccStoreRandomGen StdGen deriving Show

{-
storeRandomGen :: StdGen -> React (Singleton StoreRandomGen) ()
storeRandomGen g = result (storeRandomGen g) (pure ()) =<< await (StoreRandomGen g)
	\(OccStoreRandomGen g') -> bool Failure Succeed $ g == g'
	-}

data LoadRandomGen = LoadRandomGenReq deriving Show
numbered 4 [t| LoadRandomGen |]
instance Mrgable LoadRandomGen where lg1 `mrg` _lg2 = lg1
