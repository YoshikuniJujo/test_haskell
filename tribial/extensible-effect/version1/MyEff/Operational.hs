{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff.Operational where

import Data.Typeable

import MyEff
import Free
import TypeLevel

data Program instr v = forall a . Program (instr a) (a -> v)

instance Functor (Program instr) where
	fmap f (Program instr k) = Program instr (f . k)

singleton :: (Member (Program instr) es, Typeable instr) => instr a -> Eff es a
singleton i = Free . toUnion $ Program i Pure

runProgram :: Typeable f => (forall x . f x -> Eff es x) ->
	Eff (Program f :> es) a -> Eff es a
runProgram advent = \case
	Pure x -> Pure x
	Free u -> case fromUnion u of
		Just (Program instr k) ->
			advent instr >>= runProgram advent . k
		Nothing -> Free . fmap (runProgram advent) $ castUnion u
