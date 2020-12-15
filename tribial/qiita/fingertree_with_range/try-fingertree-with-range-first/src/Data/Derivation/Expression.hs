{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Expression where

import Prelude hiding ((<>))

import Outputable (Outputable(..), (<>), (<+>), text)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Map.Strict (Map, (!?), empty, singleton, insert)

import Control.Arrow (first, second)
import Data.Maybe
import Data.List (find)

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

type VarBool v = Map v Bool

mkVarBool :: Ord v => [Exp v Bool] -> VarBool v
mkVarBool = snd . untilFixed (uncurry vbStep) . vbInit

vbInit :: Ord v => [Exp v Bool] -> ([(v, v)], VarBool v)
vbInit [] = ([], empty)
vbInit (Var l :== Var r : es) = ((l, r) :) `first` vbInit es
vbInit (Var l :== Bool r : es) = insert l r `second` vbInit es
vbInit (Bool l :== Var r : es) = insert r l `second` vbInit es
vbInit (_ : es) = vbInit es

vbStep :: Ord v => [(v, v)] -> VarBool v -> ([(v, v)], VarBool v)
vbStep [] vb = ([], vb)
vbStep ((l, r) : vs) vb = case (vb !? l, vb !? r) of
	(Just bl, _) -> vbStep vs $ insert r bl vb
	(Nothing, Just br) -> vbStep vs $ insert l br vb
	(Nothing, Nothing) -> ((l, r) :) `first` vbStep vs vb

untilFixed :: Eq a => (a -> a) -> a -> a
untilFixed f x = fst . fromJust . find (uncurry (==)) $ zip xs (tail xs)
	where xs = iterate f x

procProp :: Ord v => VarBool v ->
	Exp v Bool -> Bool -> Writer [Constraint v] (Maybe (Constraint v))
procProp _ (Bool _) _ = pure Nothing; procProp _ (Var _) _ = pure Nothing
procProp _ (l :<= r) False = Just <$> (greatThan <$> poly l <*> poly r)
procProp _ (l :<= r) True = Just <$> (greatEqualThan <$> poly r <*> poly l)
procProp vb (l :== Bool r) b = procProp vb l (r == b)
procProp vb (Bool l :== r) b = procProp vb r (l == b)
procProp vb (l :== Var r) b | Just br <- vb !? r = case l of
	_ :== _ -> procProp vb l (br == b); _ :<= _ -> procProp vb l (br == b)
	_ -> pure Nothing
procProp vb (Var l :== r) b | Just bl <- vb !? l = case r of
	_ :== _ -> procProp vb r (bl == b); _ :<= _ -> procProp vb r (bl == b)
	_ -> pure Nothing
procProp _ (l :== r) True = case (l, r) of
	(Const _, _) -> Just <$> (equal <$> poly l <*> poly r)
	(_ :+ _, _) -> Just <$> (equal <$> poly l <*> poly r)
	(_ :- _, _) -> Just <$> (equal <$> poly l <*> poly r)
	(_, Const _) -> Just <$> (equal <$> poly l <*> poly r)
	(_, _ :+ _) -> Just <$> (equal <$> poly l <*> poly r)
	(_, _ :- _) -> Just <$> (equal <$> poly l <*> poly r)
	(Var v, Var w) -> Just <$> (equal <$> poly (Var v) <*> poly (Var w))
	_ -> pure Nothing
procProp _ (_ :== _) False = pure Nothing
