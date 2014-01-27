{-# LANGUAGE TupleSections #-}

import Prelude hiding (id, (.))

import Control.Category
import Control.Applicative

newtype IOMcn a b = IOMcn { runIOMcn :: a -> IO b }

instance Category IOMcn where
	id = IOMcn return
	IOMcn io1 . IOMcn io2 = IOMcn $ (io1 =<<) . io2

instance Functor (IOMcn a) where
	fmap f iom = pure f <*> iom
	{-
	fmap f (IOMcn io) = IOMcn $ \x -> do
		r <- io x
		return $ f r
	fmap f (IOMcn io) = IOMcn $ (f <$>) . io
	-}

instance Applicative (IOMcn a) where
	pure = IOMcn . const . return
	IOMcn p <*> IOMcn io = IOMcn $ \x -> p x <*> io x

arr :: (a -> b) -> IOMcn a b
arr = flip fmap id

putStrLnM :: IOMcn String ()
putStrLnM = IOMcn putStrLn

getLineM :: IOMcn () String
getLineM = IOMcn $ const getLine

run :: IOMcn () a -> IO a
run m = runIOMcn m ()

addLineM :: IOMcn String String
addLineM = IOMcn $ (<$> getLine) . (++)

putHello, putGoodBye :: IOMcn () ()
putHello = arr (const "Hello") >>> putStrLnM
putGoodBye = arr (const "Good-bye") >>> putStrLnM

greeting :: Bool -> IOMcn () ()
greeting True = putHello
greeting False = putGoodBye

reversible :: Bool -> IOMcn String ()
reversible True = arr reverse >>> putStrLnM
reversible False = putStrLnM

addReversible :: String -> Bool -> IOMcn String ()
addReversible add True = arr reverse >>> arr (add ++) >>> putStrLnM
addReversible add False = arr (add ++) >>> putStrLnM

app :: IOMcn (IOMcn a b, a) b
app = IOMcn $ \(IOMcn m, a) -> m a

test :: IOMcn () ()
test = arr (const True) >>> arr greeting >>> arr (, ()) >>> app

test2 :: IOMcn (Bool, String) ()
test2 = arr (\(b, str) -> (reversible b, str)) >>> app

test3 :: Bool -> String -> IOMcn () ()
test3 b str = arr (const str) >>> reversible b

test4 :: IOMcn (String, Bool, String) ()
test4 = arr (\(a, b, str) -> (addReversible a b, str)) >>> app

test5 :: String -> Bool -> String -> IOMcn () ()
test5 a b str = arr (const str) >>> addReversible a b

-- arr (const True) >>> arr greeting
--	IOMcn () Bool >>> IOMcn Bool (IOMcn () ())
--	IOMcn () (IOMcn () ())
--
-- arr (, ()) :: IOMcn (IOMcn () ()) (IOMcn () (), ())
--
-- a -> IOMcn b c
-- (a, b) -> (IOMcn b c, b)
-- IOMcn (a, b) (IOMcn b c, b)
-- IOMcn (a, b) c
