{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Expression where

import Prelude hiding ((<>))

import GHC.Utils.Outputable (Outputable(..), (<>), (<+>), text)

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
