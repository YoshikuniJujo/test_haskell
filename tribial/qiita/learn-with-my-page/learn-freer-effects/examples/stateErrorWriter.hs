{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow

import Freer

data SE s e w a where
	Get :: SE s e w s
	Put :: s -> SE s e w ()
	Exc :: e -> SE s e w a
	Writer :: w -> SE s e w ()

get :: Freer (SE s e w) s
get = freer Get

put :: s -> Freer (SE s e w) ()
put = freer . Put

modify :: (s -> s) -> Freer (SE s e w) ()
modify f = put . f =<< get

throwError :: e -> Freer (SE s e w) a
throwError = freer . Exc

catchError :: Freer (SE s e w) a -> (e -> Freer (SE s e w) a) -> Freer (SE s e w) a
m `catchError` h = case m of
	Pure x -> return x
	Exc e `Bind` _k -> h e
	mx `Bind` k -> mx `Bind` ((`catchError` h) . k)

runState :: Freer (SE s e w) a -> s -> Freer (SE s e w) (a, s)
runState m s = case m of
	Pure x -> Pure (x, s)
	Get `Bind` k -> runState (k s) s
	Put s' `Bind` k -> runState (k ()) s'
	mx `Bind` k -> mx `Bind` ((`runState` s) . k)

runError :: Freer (SE s e w) a -> Freer (SE s e w) (Either e a)
runError = \case
	Pure x -> Pure $ Right x
	Exc e `Bind` _k -> return $ Left e
	mx `Bind` k -> mx `Bind` (runError . k)

runPure :: Freer (SE s e w) a -> a
runPure = \case
	Pure x -> x
	_ -> error "remain State or Error"

safeDiv :: Integer -> Integer -> Freer (SE s String String) Integer
safeDiv n 0 = throwError $ show n ++ " is divided by 0"
safeDiv n m = do
	tell $ show n ++ " `div` " ++ show m ++ "\n"
	return $ n `div` m

sample :: Freer (SE Integer String String) Integer
sample = do
	a <- get
	modify (subtract 5)
	b <- get
	c <- 60 `safeDiv` b
	put a
	modify (subtract 3)
	d <- get
	e <- 250 `safeDiv` d
	return $ c + e

tell :: w -> Freer (SE s e w) ()
tell = freer . Writer

runWriter :: Monoid w => Freer (SE s e w) a -> Freer (SE s e w) (a, w)
runWriter = \case
	Pure x -> return (x, mempty)
	Writer w `Bind` k -> second (w <>) <$> runWriter (k ())
	mx `Bind` k -> mx `Bind` (runWriter . k)
