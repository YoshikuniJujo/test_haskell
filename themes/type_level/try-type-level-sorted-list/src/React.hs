{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module React where

import Prelude hiding (head, tail, filter)

import Data.Kind
import Data.Bool
import Data.Maybe
import Data.List.NonEmpty
import Data.Time

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

adjust :: forall es es' a . (
	Merge es es' ~ es', Merge es' es ~ es', Convert es es', Convert es' es',
	Convert (Map Occurred es') (Map Occurred es),
	Convert (Map Occurred es) (Map Occurred es'),
	Convert (Map Occurred es') (Map Occurred es') ) => React es a -> React es' a
adjust rct = (rct `first` (ignore :: React es' ())) >>= \case
	(Done x, _) -> pure x
	(rct', _) -> adjust rct'

ignore :: React es ()
ignore = Await [] $ const ignore

first :: forall es es' a b . (
	Merge es es' ~ Merge es' es,
	Convert es (Merge es' es),
	Convert es' (Merge es' es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es')
	) => React es a -> React es' b -> React (Merge es es') (React es a, React es' b)
l `first` r = case (l, r) of
	(Await el _, Await er _) ->
		Await ((fromJust . convert <$> el) ++ (fromJust . convert <$> er))
			\(c :: EvOccs (Merge es es')) -> (l `ud1` c) `first` (r `ud2` c)
	_ -> Done (l, r)
	where
	ud1 = update @es @es'
	ud2 = update @es' @es

update :: Convert (Map Occurred (Merge es es')) (Map Occurred es) => React es a -> EvOccs (Merge es es') -> React es a
update r@(Await _ c) oc = case fromJust <$> filter isJust (convert <$> oc) of
	o : os -> c $ o :| os
	[] -> r
update _ _ = error "bad: first argument must be Await _ _"

-- type GuiEv = Insert MouseDown (Singleton MouseUp)

data MouseDown = MouseDownReq deriving Show

numbered [t| MouseDown |]
instance Request MouseDown where
	data Occurred MouseDown = OccurredMouseDown [MouseBtn] deriving Show

data MouseBtn = MLeft | MMiddle | MRight | MUp | MDown deriving (Show, Eq)

mouseDown :: React (Singleton MouseDown) [MouseBtn]
mouseDown = Await [inj MouseDownReq] \ev ->
	let OccurredMouseDown mbs = extract $ head ev in pure mbs

data MouseUp = MouseUpReq deriving Show

numbered [t| MouseUp |]
instance Request MouseUp where
	data Occurred MouseUp = OccurredMouseUp [MouseBtn] deriving Show

data TryWait = TryWaitReq DiffTime deriving Show

numbered [t| TryWait |]
instance Request TryWait where
	data Occurred TryWait = OccurredTryWait DiffTime deriving Show

tryWait :: DiffTime -> React (Singleton TryWait) DiffTime
tryWait t = Await [inj $ TryWaitReq t] \ev ->
	let OccurredTryWait t' = extract $ head ev in pure t'

sleep :: DiffTime -> React (Singleton TryWait) ()
sleep t = do
	t' <- tryWait t
	if t' == t then pure () else sleep (t - t')

type ReactG = React GuiEv
type GuiEv = MouseDown `Insert` (MouseUp `Insert` Singleton TryWait)

mouseUp :: React (Singleton MouseUp) [MouseBtn]
mouseUp = Await [inj MouseUpReq] \ev ->
	let OccurredMouseUp mbs = extract $ head ev in pure mbs

clickOn :: MouseBtn -> React (Singleton MouseDown) ()
clickOn b = mouseDown >>= bool (clickOn b) (pure ()) . (b `elem`)

leftClick, middleClick, rightClick :: React (Singleton MouseDown) ()
[leftClick, middleClick, rightClick] = clickOn <$> [MLeft, MMiddle, MRight]

releaseOn :: MouseBtn -> React (Singleton MouseUp) ()
releaseOn b = mouseUp >>= bool (releaseOn b) (pure ()) . (b `elem`)

leftUp, middleUp, rightUp :: React (Singleton MouseUp) ()
[leftUp, middleUp, rightUp] = releaseOn <$> [MLeft, MMiddle, MRight]

sameClick :: ReactG Bool
sameClick = do
	pressed <- adjust mouseDown
	pressed2 <- adjust mouseDown
	pure $ pressed == pressed2

filterEvent' :: EvOccs es -> EvReqs es -> [UnionValue (Map Occurred es)]
filterEvent' = intersection'' @Occurred

filterEvent :: [UnionValue (Map Occurred es)] -> EvReqs es -> [UnionValue (Map Occurred es)]
filterEvent = intersection' @Occurred

before :: (
	Merge es es' ~ Merge es' es, Convert es (Merge es' es), Convert es' (Merge es' es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es') ) =>
	React es a -> React es' b -> React (Merge es es') Bool
before a b = do
	(a', b') <- a `first` b
	case (done a', done b') of
		(Just _, Nothing) -> pure True
		_ -> pure False

done :: React es a -> Maybe a
done (Done x) = Just x
done _ = Nothing

doubler :: ReactG ()
doubler = do
	r <- adjust do
		adjust rightClick
		rightClick `before` sleep 0.2
	if r then pure () else doubler
