{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ExampleLifeHierarchy where

import Data.Typeable

import SomeLife
import LifeHierarchy

-- data Foo

data Bacteria = Bacteria deriving (Typeable, Show)
newtype Dog = Dog String deriving (Typeable, Show)
newtype Cat = Cat String deriving (Typeable, Show)
data Ash = Ash deriving (Typeable, Show)
data Dandelion = Dandelion deriving (Typeable, Show)

lifeHierarchy Nothing (LifeType ''Bacteria)
lifeHierarchy Nothing $ LifeNode "Animal" [LifeType ''Dog, LifeType ''Cat]
lifeHierarchy Nothing $ LifeNode "Plant" [
	LifeNode "Tree"  [LifeType ''Ash],
	LifeNode "Grass" [] ]

lifeHierarchy (Just ''Grass) $ LifeType ''Dandelion

{-
lifeContainer $ mkName "Foo"

data Bar = Bar deriving Show

(: []) <$> instLife ''Foo ''Bar

data Baz = Baz deriving Show

(: []) <$> defInstLife ''Foo
-}
