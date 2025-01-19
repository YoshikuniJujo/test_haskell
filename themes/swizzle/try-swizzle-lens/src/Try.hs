module Try where

newtype Const c a = Const { runConst :: c } deriving Show

instance Functor (Const c) where _ `fmap` Const c = Const c

newtype Identity a = Identity { runIdentity :: a } deriving Show

instance Functor Identity where f `fmap` Identity x = Identity $ f x
