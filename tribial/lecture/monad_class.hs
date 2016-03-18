import Control.Applicative

safeDivM :: Int -> Int -> Maybe Int
_ `safeDivM` 0 = Nothing
x `safeDivM` y = Just $ x `div` y

calcM :: Int -> Int -> Int -> Maybe Int
calcM a b c =
	a `safeDivM` b >>= \x ->
	x `safeDivM` c

data Try a = Error String | Success a deriving Show

instance Functor Try where
	fmap = (=<<) . (return .)

instance Applicative Try where
	pure = return
	tf <*> tx =
		tf >>= \f ->
		tx >>= \x ->
		return $ f x

instance Monad Try where
	return = Success
	Error em >>= _ = Error em
	Success x >>= f = f x

safeDivE :: Int -> Int -> Try Int
x `safeDivE` 0 = Error $ show x ++ " is divided by zero\n"
x `safeDivE` y = Success $ x `div` y

calcE :: Int -> Int -> Int -> Try Int
calcE a b c =
	a `safeDivE` b >>= \x ->
	x `safeDivE` c
