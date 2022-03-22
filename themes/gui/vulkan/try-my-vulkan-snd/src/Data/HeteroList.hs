{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.HeteroList (
	Tip(..), (:.:)(..), length, StorableList(..) ) where

import Prelude hiding (length)

import Foreign.Storable

infixr 5 :.:

data a :.: b = a :.: b deriving Show

data Tip = Tip

{-# COMPLETE (:.:) #-}

class Length vs where length :: vs -> Int

instance Length Tip where length _ = 0

instance Length ts => Length (t :.: ts) where
	length (_ :.: vs) = 1 + length vs

class StorableList vs where sizeAlignments :: vs -> [(Int, Int)]

instance StorableList () where sizeAlignments _ = []
instance StorableList Tip where sizeAlignments _ = []
instance (Storable t, StorableList ts) => StorableList (t :.: ts) where
	sizeAlignments (x :.: xs) = (sizeOf x, alignment x) : sizeAlignments xs
