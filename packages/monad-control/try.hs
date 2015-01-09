{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses,
	UndecidableInstances,
	PackageImports #-}

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.Identity
import "monads-tf" Control.Monad.State
import Control.Monad.Base
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control

data MyTrans m a = MyTrans { runMyTrans :: m a } deriving Show

instance Functor m => Functor (MyTrans m) where
	fmap f = MyTrans . fmap f . runMyTrans

instance Applicative m => Applicative (MyTrans m) where
	pure = MyTrans . pure
	(<*>) (MyTrans f) = MyTrans . (<*>) f . runMyTrans

instance Monad m => Monad (MyTrans m) where
	return = MyTrans . return
	(MyTrans m) >>= f = MyTrans $ m >>= runMyTrans . f

instance MonadTrans MyTrans where
	lift = MyTrans

instance MonadTransControl MyTrans where
	newtype StT MyTrans a = StMyTrans { unStMyTrans :: a }
	liftWith f = MyTrans $ f $ liftM StMyTrans . runMyTrans
	restoreT = MyTrans . liftM unStMyTrans

instance (MonadBase b m) => MonadBase b (MyTrans m) where
	liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (MyTrans m) where
	newtype StM (MyTrans m) a = StMMyTrans {
		unStMMyTrans :: ComposeSt MyTrans m a }
	liftBaseWith = defaultLiftBaseWith StMMyTrans
	restoreM = defaultRestoreM unStMMyTrans
