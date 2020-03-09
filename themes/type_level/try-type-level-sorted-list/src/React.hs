{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module React where

import Data.Kind

import Sorted
import OpenUnionValue

type EvReqs (es :: Sorted Type) = [UnionValue es]
type EvOcc (es :: Sorted Type) = UnionValue (Map Occurred es)

-- type family Occurred :: Type -> Type
-- type family Occurred (a :: Type) :: Type

-- type instance Occurred Int = Bool

class Numbered e => Request e where
	data Occurred (e :: Type) :: Type

data React (es :: Sorted Type) a
	= Done a
	| Await (EvReqs es) (EvOcc es -> React es a)

instance Functor (React es) where
	f `fmap` Done x = Done $ f x
	f `fmap` Await reqs k = Await reqs \occ -> f `fmap` k occ

instance Applicative (React es) where
	pure = Done
	Done f <*> mx = f <$> mx
	Await reqs kf <*> mx = Await reqs $ (>>= (<$> mx)) . kf

instance Monad (React es) where
	Done x >>= f = f x
	Await reqs k >>= f = Await reqs $ (>>= f) . k

newtype Sig es a b = Sig (React es (ISig es a b))
data ISig es a b = a :| Sig es a b | End b

data MouseDown = MouseDownReq

instance Numbered MouseDown where
	type Number MouseDown = 0

instance Request MouseDown where
	data Occurred MouseDown = OccurredMouseDown [MouseBtn]

data MouseBtn = MLeft | MMiddle | MRight | MUp | MDown deriving (Show, Eq)

mouseDown :: React (Singleton MouseDown) [MouseBtn]
mouseDown = Await ([inj MouseDownReq]) \ev ->
	let OccurredMouseDown mbs = extract ev in pure mbs

interpret :: Monad m => (EvReqs es -> m [EvOcc es]) -> React es a -> m a
interpret _ (Done x) = pure x
interpret p a@(Await r _) = p r >>= interpret p . foldr step a

step :: EvOcc es -> React es a -> React es a
step _ d@(Done _) = d
step occ (Await _ c) = c occ

sameClick :: React (Singleton MouseDown) Bool
sameClick = do
	pressed <- mouseDown
	pressed2 <- mouseDown
	pure $ pressed == pressed2

clickOn :: MouseBtn -> React (Singleton MouseDown) ()
clickOn b = do
	bs <- mouseDown
	if b `elem` bs then pure () else clickOn b

leftClick, middleClick, rightClick :: React (Singleton MouseDown) ()
leftClick = clickOn MLeft
middleClick = clickOn MMiddle
rightClick = clickOn MRight
