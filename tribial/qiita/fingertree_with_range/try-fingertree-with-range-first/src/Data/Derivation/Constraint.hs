{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Constraint where

import Prelude hiding (null, filter, (<>))

import Outputable (Outputable(..), (<>), (<+>), text)
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Map.Strict (Map, null, singleton, (!?), filter, lookupMin)
import Data.Map.Merge.Strict

import qualified Data.Map.Strict as M

type Polynomial v = Map (Maybe v) Integer

(.+), (.-) :: Ord v => Polynomial v -> Polynomial v -> Polynomial v
(.+) = merge preserveMissing preserveMissing
	(zipWithMaybeMatched \_ a b -> rmZero $ a + b)
(.-) = merge preserveMissing (mapMissing $ const negate)
	(zipWithMaybeMatched \_ a b -> rmZero $ a - b)

rmZero :: (Eq n, Num n) => n -> Maybe n
rmZero = \case 0 -> Nothing; n -> Just n

data Constraint v = Eq (Polynomial v) | Geq (Polynomial v)
	deriving (Show, Eq, Ord)
