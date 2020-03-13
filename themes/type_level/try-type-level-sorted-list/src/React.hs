{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module React where

import Data.Kind
import Data.Maybe
import Data.Time

import Sorted
import OpenUnionValue

type EvReqs (es :: Sorted Type) = [UnionValue es]
type EvOcc (es :: Sorted Type) = UnionValue (Map Occurred es)

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

data MouseDown = MouseDownReq

instance Numbered MouseDown where type Number MouseDown = 0
instance Request MouseDown where
	data Occurred MouseDown = OccurredMouseDown [MouseBtn]

data MouseBtn = MLeft | MMiddle | MRight | MUp | MDown deriving (Show, Eq)

mouseDown :: React (Singleton MouseDown) [MouseBtn]
mouseDown = Await [inj MouseDownReq] \ev ->
	let OccurredMouseDown mbs = extract ev in pure mbs

data MouseUp = MouseUpReq

instance Numbered MouseUp where type Number MouseUp = 1
instance Request MouseUp where
	data Occurred MouseUp = OccurredMouseUp [MouseBtn]

mouseUp :: React (Singleton MouseUp) [MouseBtn]
mouseUp = Await [inj MouseUpReq] \ev ->
	let OccurredMouseUp mbs = extract ev in pure mbs

data TryWait = TryWaitReq DiffTime deriving Show

instance Numbered TryWait where type Number TryWait = 2
instance Request TryWait where
	data Occurred TryWait = OccurredTryWait DiffTime

tryWait :: DiffTime -> React (Singleton TryWait) DiffTime
tryWait t = Await [inj $ TryWaitReq t] \ev ->
	let OccurredTryWait t' = extract ev in pure t'

sleep :: DiffTime -> React (Singleton TryWait) ()
sleep t = do
	t' <- tryWait t
	if t' == t then pure () else sleep (t - t')

data MouseMove = MouseMoveReq deriving Show

instance Numbered MouseMove where type Number MouseMove = 3
instance Request MouseMove where
	data Occurred MouseMove = OccurredMouseMove Point

type Point = (Integer, Integer)

mouseMove :: React (Singleton MouseMove) Point
mouseMove = Await [inj MouseMoveReq] \ev ->
	let OccurredMouseMove p = extract ev in pure p

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

releaseOn :: MouseBtn -> React (Singleton MouseUp) ()
releaseOn b = do
	bs <- mouseUp
	if b `elem` bs then pure () else releaseOn b

leftRelease :: React (Singleton MouseUp) ()
leftRelease = releaseOn MLeft

first :: forall es es' a b . (
	Merge es es' ~ Merge es' es,
	Convert es (Merge es' es),
	Convert es' (Merge es' es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es')
	) => React es a -> React es' b -> React (Merge es es') (React es a, React es' b)
first l r = case (l, r) of
	(Await el _, Await er _) ->
		Await ((fromJust . convert <$> el) ++ (fromJust . convert <$> er))
			\(c :: EvOcc (Merge es es')) -> first (l `ud1` c) (r `ud2` c)
	_ -> Done (l, r)
	where
	ud1 = update @es @es'
	ud2 = update @es' @es

update :: Convert (Map Occurred (Merge es es')) (Map Occurred es) => React es a -> EvOcc (Merge es es') -> React es a
update r@(Await _ c) oc = case convert oc of Just o -> c o; Nothing -> r
update _ _ = error "bad: first argument must be Await _ _"

done :: React es a -> Maybe a
done (Done x) = Just x
done _ = Nothing

before :: (Merge es es' ~ Merge es' es, Convert es (Merge es' es), Convert es' (Merge es' es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es),
	Convert (Map Occurred (Merge es' es)) (Map Occurred es')) =>
	React es a -> React es' b -> React (Merge es es') Bool
before a b = do
	(a', b') <- first a b
	case (done a', done b') of
		(Just _, Nothing) -> pure True
		_ -> pure False

-- infixr 5 `Insert`

type GuiEv = MouseMove `Insert` (MouseDown `Insert` (MouseUp `Insert` Singleton TryWait))
type ReactG a = React GuiEv a

doubler :: ReactG ()
doubler = do
	r <- adjust do
		adjust rightClick
		rightClick `before` sleep 0.2
--		rightClick `before` sleep 2
--	pure ()
	if r then pure () else doubler

filterEvent :: [EvOcc es] -> EvReqs es -> [EvOcc es]
filterEvent = intersection' @Occurred

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
