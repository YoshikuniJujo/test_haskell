{-# LANGUAGE ExistentialQuantification #-}

import Data.Typeable

data Some = forall a . (Typeable a, Show a) => Some a

instance Show Some where
	show (Some x) = "Some " ++ show x

newtype Id x = Id x deriving Show

eight :: Some
eight = Some (8 :: Int)

data Foo a = forall t . Typeable t => Foo (t a)

list :: Foo Char
list = Foo "hello"

fromFoo :: Typeable t => Foo a -> Maybe (t a)
fromFoo foo = case foo of
	Foo x -> fmap (\(Id x) -> x) $ gcast1 (Id x)
