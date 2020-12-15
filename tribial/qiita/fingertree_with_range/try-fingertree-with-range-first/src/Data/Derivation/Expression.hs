{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Expression where

import Prelude hiding ((<>))

import Outputable (Outputable(..), (<>), (<+>), text)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Map.Strict (Map, (!?), empty, singleton, insert)

import Data.Derivation.Constraint

data Exp v t where
	Bool :: Bool -> Exp v Bool
	Var :: v -> Exp v a
	Const :: Integer -> Exp v Number
	(:==) :: Exp v a -> Exp v a -> Exp v Bool
	(:<=) :: Exp v Number -> Exp v Number -> Exp v Bool
	(:+) :: Exp v Number -> Exp v Number -> Exp v Number
	(:-) :: Exp v Number -> Exp v Number -> Exp v Number

data Number

deriving instance Show v => Show (Exp v t)

instance Outputable v => Outputable (Exp v t) where
	ppr (Bool b) = text "(Bool" <+> ppr b <> text ")"
	ppr (Var v) = text "(Var" <+> ppr v <> text ")"
	ppr (Const n) = text "(Const" <+> ppr n <> text ")"
	ppr (l :== r) = text "(" <> ppr l <+> text ":==" <+> ppr r <> text ")"
	ppr (l :<= r) = text "(" <> ppr l <+> text ":<=" <+> ppr r <> text ")"
	ppr (l :+ r) = text "(" <> ppr l <+> text ":+" <+> ppr r <> text ")"
	ppr (l :- r) = text "(" <> ppr l <+> text ":-" <+> ppr r <> text ")"

poly :: Ord v => Exp v Number -> Writer [Constraint v] (Polynomial v)
poly (Const n) = pure $ singleton Nothing n
poly (Var v) = p <$ tell [p `greatEqualThan` empty]
	where p = singleton (Just v) 1
poly (l :+ r) = (.+) <$> poly l <*> poly r
poly (l :- r) = (,) <$> poly l <*> poly r >>= \(pl, pr) ->
	pl .- pr <$ tell [pl `greatEqualThan` pr]
