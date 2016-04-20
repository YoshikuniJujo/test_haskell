{-# LANGUAGE MonadComprehensions #-}

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
	fmap = (=<<) . (return .)

instance Applicative (State s) where
	pure = State . (,)
	mf <*> mx = [ f x | f <- mf, x <- mx ]

instance Monad (State s) where
	State m >>= f = State $ \s -> let (x, s') = m s in runState (f x) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= put . f

data Operation
	= E Expression
	| ErasePreLine
	deriving Show

data Expression
	= Expression :+: Expression
	| Expression :-: Expression
	| Expression :*: Expression
	| I Integer
	deriving Show

type Result = (Operation, Maybe Integer)

showExp :: Expression -> String
showExp (n1 :+: n2) = "(" ++ showExp n1 ++ " + " ++ showExp n2 ++ ")"
showExp (n1 :-: n2) = "(" ++ showExp n1 ++ " - " ++ showExp n2 ++ ")"
showExp (n1 :*: n2) = "(" ++ showExp n1 ++ " * " ++ showExp n2 ++ ")"
showExp (I n) = show n

evaluate :: Expression -> Integer
evaluate (o1 :+: o2) = evaluate o1 + evaluate o2
evaluate (o1 :-: o2) = evaluate o1 - evaluate o2
evaluate (o1 :*: o2) = evaluate o1 * evaluate o2
evaluate (I n) = n

operate :: Operation -> State [String] Result
operate o@(E e) = do
	modify ((showExp e ++ " = " ++ show r) :)
	return (o, Just r)
	where
	r = evaluate e
operate ErasePreLine = do
	modify (\ls -> if null ls then [] else tail ls)
	return (ErasePreLine, Nothing)

operateAll :: [Operation] -> State [String] [Result]
operateAll = traverse operate
