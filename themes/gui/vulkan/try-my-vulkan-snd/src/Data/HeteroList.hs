{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs, TypeFamilies, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.HeteroList (
	Tip(..), (:.:)(..), length, StorableList(..), HeteroList(..) ) where

import Prelude hiding (length)

import Foreign.Storable
import Data.Kind

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

infixr 5 :..:

data HeteroList (as :: [Type]) where
	HNil :: HeteroList '[]
	(:..:) :: a -> HeteroList as -> HeteroList (a ': as)

instance Show (HeteroList '[]) where show HNil = "HNil"

instance (Show a, Show (HeteroList as)) => Show (HeteroList (a ': as)) where
	show (x :..: xs) = show x ++ " :..: " ++ show xs
