{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
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
createMessage str f = do
	putStrLn "before"
	f (Message str) <* putStrLn "after"

-- class CreateMessages ss where
--	createMessages :: [String] -> (forall ss .

fooMessages :: [String] ->
	(forall ss . Show (List Message ss) => List Message ss -> IO a) -> IO a
fooMessages [] f = f Nil
fooMessages (s : ss) f = createMessage s \msg -> fooMessages ss \msgs -> f $ msg :. msgs

indexMessage :: List Message ss -> Int -> (forall s . Message s -> IO a) -> IO a
indexMessage (x :. _) 0 f = f x
indexMessage (_ :. xs) i f | i > 0 = indexMessage xs (i - 1) f

copyMessage :: Message s -> Message t -> Message t
copyMessage (Message str) _ = Message str

createFromMessage :: Message s -> (forall t . Message t -> IO a) -> IO a
createFromMessage m@(Message str) f = do
	print m
	f $ Message str

escape :: String -> IO ((forall s . Message s -> IO a) -> IO a)
escape str = createMessage str \msg1 ->
	pure $ createFromMessage msg1

foo :: IO (forall s . s -> a)
foo = pure undefined

bar :: IO ((forall s . Message s -> IO a) -> IO a)
bar = pure $ createMessage "hello"
