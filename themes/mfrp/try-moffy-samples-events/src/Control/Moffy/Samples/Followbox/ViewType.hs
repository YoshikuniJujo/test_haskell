{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Followbox.ViewType (
	-- * VIEW
	View(..), View1,
	-- * COLOR
	Color(..), white, blue, Png(..),
	-- * TEMP
	VText(..), Line(..), Image(..)
	) where

import Control.Moffy.Samples.Viewable.Basic
import Control.Moffy.Samples.Viewable.Text
import Control.Moffy.Samples.Viewable.Shape
import Control.Moffy.Samples.Viewable.Image
import Data.Type.Set
import Data.OneOfThem

---------------------------------------------------------------------------

newtype View = View [View1] deriving Show

type View1 = OneOfThem (VText :- Line :- Image :- 'Nil)

instance Semigroup View where View vs1 <> View vs2 = View $ vs1 <> vs2
instance Monoid View where mempty = View []
