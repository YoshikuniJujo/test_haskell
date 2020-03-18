{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module React where

import Prelude hiding (head, tail)

import Data.Kind
import Data.Bool
import Data.List.NonEmpty

import Sorted
import OpenUnionValue

type EvReqs (es :: Sorted Type) = [UnionValue es]
type EvOccs (es :: Sorted Type) = NonEmpty (UnionValue (Map Occurred es))

class Numbered e => Request e where data Occurred (e :: Type) :: Type

data React (es :: Sorted Type) a =
	Done a | Await (EvReqs es) (EvOccs es -> React es a)

instance Functor (React es) where
	f `fmap` Done x = Done $ f x
	f `fmap` Await reqs k = Await reqs \occs -> f `fmap` k occs

instance Applicative (React es) where
	pure = Done
	Done f <*> mx = f <$> mx
	Await reqs kf <*> mx = Await reqs $ (>>= (<$> mx)) . kf

instance Monad (React es) where
	Done x >>= f = f x
	Await reqs k >>= f = Await reqs $ (>>= f) . k

interpret :: Monad m => (EvReqs es -> m (EvOccs es)) -> React es a -> m a
interpret _ (Done x) = pure x
interpret p (Await r c) = interpret p . c =<< p r

type GuiEv = Singleton MouseDown
type ReactG = React GuiEv

data MouseDown = MouseDownReq deriving Show

numbered [t| MouseDown |]
instance Request MouseDown where
	data Occurred MouseDown = OccurredMouseDown [MouseBtn] deriving Show

data MouseBtn = MLeft | MMiddle | MRight | MUp | MDown deriving (Show, Eq)

mouseDown :: React (Singleton MouseDown) [MouseBtn]
mouseDown = Await [inj MouseDownReq] \ev ->
	let OccurredMouseDown mbs = extract $ head ev in pure mbs

clickOn :: MouseBtn -> React (Singleton MouseDown) ()
clickOn b = mouseDown >>= bool (clickOn b) (pure ()) . (b `elem`)

leftClick, middleClick, rightClick :: React (Singleton MouseDown) ()
[leftClick, middleClick, rightClick] = clickOn <$> [MLeft, MMiddle, MRight]

sameClick :: ReactG Bool
sameClick = do
	pressed <- mouseDown
	pressed2 <- mouseDown
	pure $ pressed == pressed2
