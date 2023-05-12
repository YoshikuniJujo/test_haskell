{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryHeteroMap where

import Data.Kind

infixr 5 :.

-- data List (t :: k -> Type) ss where
data List t ss where
	Nil :: List t '[]
	(:.) :: t s -> List t ss -> List t (s ': ss)

instance Show (List t '[]) where show Nil = "Nil"

instance (Show (t s), Show (List t ss)) => Show (List t (s ': ss)) where
	show (t :. ts) = show t ++ " :. " ++ show ts

data Message s = Message String deriving Show

makeMessage :: String -> (forall s . Message s -> IO a) -> IO a
makeMessage str f = do
	putStrLn "MAKE MESSAGE"
	f (Message str)
		<* putStrLn "FREE MESSAGE"

makeMessage2 :: String -> String ->
	(forall s t . Message s -> Message t -> IO a) -> IO a
makeMessage2 s1 s2 f =
	makeMessage s1 \m1 -> makeMessage s2 \m2 -> f m1 m2

makeMessage3, makeMessage3', makeMessage3'' :: String -> String -> String ->
	(forall s t u . Message s -> Message t -> Message u -> IO a) -> IO a
makeMessage3 s1 s2 s3 f =
	makeMessage s1 \m1 -> makeMessage s2 \m2 -> makeMessage s3 \m3 ->
	f m1 m2 m3

makeMessage3' s1 s2 s3 f =
	makeMessage s1 \m1 -> makeMessage2 s2 s3 \m2 m3 -> f m1 m2 m3

makeMessage3'' s1 s2 s3 f =
	makeMessage s3 \m3 -> makeMessage2 s1 s2 \m1 m2 -> f m1 m2 m3

-- type AddArg f = forall s . Message s -> f

class Foo f where foo :: f -> IO ()

instance Foo (IO ()) where foo = id

-- instance Foo f => Foo (forall s . Message s -> f) where
instance Foo f => Foo (String -> f) where

data String' (d :: ()) = String' String deriving Show

{-
class MakeMessages ds f where
	makeMessages ::
--		List String' ds -> (forall (ss :: [Type]) . List Message ss -> IO a) -> IO a
		List String' ds -> f

-- instance MakeMessages '[] ((List Message '[] -> IO a) -> IO a) where makeMessages Nil f = f Nil

instance MakeMessages '[] (IO a -> IO a) where makeMessages Nil f = f
-}

{-
instance MakeMessages ds fs =>
	MakeMessages (d ': ds) (forall s . Message s 
	-}

-- (forall s . Message s -> IO a) -> IO a
-- (forall t . (forall s . Message t -> Message s -> IO a) -> IO a

{-
makeMakeMessage2 ::
	(t1 -> (Message s -> t2) -> t3) ->
	(t4 -> (Message t5 -> IO a) -> t2) ->
	t1 -> t4 ->
	(forall s1 t6 . Message s1 -> Message t6 -> IO a) -> t3
	-}
makeMakeMessage2 fun1 fun2 s1 s2 (f :: forall s t . Message s -> Message t -> IO a) =
	fun1 s1 \m1 -> fun2 s2 \m2 -> f m1 m2

-- makeMessage2' = makeMakeMessage2 makeMessage makeMessage

{-
foo ::	(forall a . (forall s . Message s -> IO a) -> IO a) ->
	(forall b . (forall t . Message t -> IO b) -> IO b) ->
	(forall s t . Message s -> Message t -> IO c) -> IO c
foo s t f = s \x -> t \y -> f x y
-}

{-
instance MakeMessages ds fs =>
	MakeMessages (d ': ds) ((forall s . 
	-}

{-
instance MakeMessages ds => MakeMessages ('() ': ds) where
	makeMessages (String' str :. ss) f =
		makeMessage str \(msg :: Message s) ->
		makeMessages ss \(msgs :: List Message ss) ->
		f @(s ': ss) $ msg :. msgs
		-}

makeMessageRaw :: String -> (String -> IO a) -> IO a
makeMessageRaw str f = f str

makeMessageRaws :: [String] -> ([String] -> IO a) -> IO a
makeMessageRaws [] f = f []
makeMessageRaws (str : strs) f =
	makeMessageRaw str \s -> makeMessageRaws strs \ss -> f $ s : ss

-- (forall t u . Foo t -> Foo u -> IO a) -> IO a
-- => (forall s t u . Foo s -> Foo t -> Foo u -> IO a) -> IO a

{-
addValue ::
	((forall t u . Message t -> Message u -> IO a) -> IO a) ->
	(forall s t u . Message s -> Message t -> Message u -> IO b) -> IO b
addValue fun k
-}
