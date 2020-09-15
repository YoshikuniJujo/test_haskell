{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.ViewType (
	-- * VIEW
	View(..), View1,
	-- * COLOR
	Color(..), white, blue, Png(..),
	-- * TEMP
	VText(..), Line(..), Image(..)
	) where

import Data.Type.Set
import Data.OneOfThem

import Trial.Followbox.Color
import Trial.Followbox.Text
import Trial.Followbox.Shape
import Trial.Followbox.Image

---------------------------------------------------------------------------

newtype View = View [View1]

type View1 = OneOfThem (VText :- Line :- Image :- 'Nil)

instance Semigroup View where View vs1 <> View vs2 = View $ vs1 <> vs2
instance Monoid View where mempty = View []
