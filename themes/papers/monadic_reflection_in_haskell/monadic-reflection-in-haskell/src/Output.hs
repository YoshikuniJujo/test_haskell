{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Output where

data Output a = Output {
	returnValue :: a,
	outputString :: String
	} deriving Show

instance Functor Output where fmap f (Output x s) = Output (f x) s

instance Applicative Output where
	pure x = Output x ""
	Output f s <*> Output x s' = Output (f x) (s ++ s')

instance Monad Output where
	m >>= f = let Output a s = m; Output b s' = f a in Output b (s ++ s')

out :: Char -> Output ()
out c = Output () [c]

collect :: Output () -> String
collect = outputString

data Output' a = Output' { getOutput' :: String -> (a, String) }

instance Functor Output' where fmap f (Output' g) = Output' \s -> let (x, s') = g s in (f x, s')

instance Applicative Output' where
	pure x = Output' (\s -> (x, s))
	Output' mf <*> Output' mx = Output' \s -> let
		(f, s') = mf s
		(x, s'') = mx s' in (f x, s'')

instance Monad Output' where
	m >>= f = Output' (\s -> let (a, s') = getOutput' m s in getOutput' (f a) s')

out' :: Char -> Output' ()
out' c = Output' (\s -> ((), c : s))

collect' :: Output' () -> String
collect' m = let ((), s) = getOutput' m "" in reverse s

flush :: Output' ()
flush = Output' (\_ -> ((), ""))

peek :: Output' Char
peek = Output' (\s -> (head s, s))

reflectO :: Output a -> Output' a
reflectO m = let Output a s = m in Output' (\s' -> (a, (reverse s) ++ s'))

reifyO :: Output' a -> Output a
reifyO m' = uncurry Output (let (a, s) = getOutput' m' [] in (a, reverse s))
