{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LocalValues where

newtype Message s = Message String deriving Show

infixr 5 :.

data List t ss where
	Nil :: List t '[]
	(:.) :: t s -> List t ss -> List t (s ': ss)

instance Show (List t '[]) where show Nil = "Nil"

instance (Show (t s), Show (List t ss)) => Show (List t (s ': ss)) where
	show (x :. xs) = show x ++ " :. " ++ show xs

createMessage :: String -> (forall s . Message s -> IO a) -> IO a
createMessage str f = f $ Message str

-- class CreateMessages ss where
--	createMessages :: [String] -> (forall ss .

fooMessages :: [String] ->
	(forall ss . Show (List Message ss) => List Message ss -> IO a) -> IO a
fooMessages [] f = f Nil
fooMessages (s : ss) f = createMessage s \msg -> fooMessages ss \msgs -> f $ msg :. msgs

indexMessage :: List Message ss -> Int -> (forall s . Message s -> IO a) -> IO a
indexMessage (x :. _) 0 f = f x
indexMessage (_ :. xs) i f | i > 0 = indexMessage xs (i - 1) f
