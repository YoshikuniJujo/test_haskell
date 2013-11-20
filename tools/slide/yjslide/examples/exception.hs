{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}

import Control.Exception
import Data.Typeable

data MyException = ThisException | ThatException deriving (Show, Typeable)

instance Exception MyException

this :: Maybe MyException
this = case toException ThisException of
	SomeException e -> cast e

data JException = forall j . Exception j => JException j deriving Typeable
instance Show JException where
	show (JException j) = show j

jToException :: Exception e => e -> SomeException
jToException = toException . JException

jFromException :: Exception e => SomeException -> Maybe e
jFromException x = do
	JException j <- fromException x
	cast j

data YJException = YJException deriving (Show, Typeable)
data MJException = MJException deriving (Show, Typeable)
data IJException = IJException deriving (Show, Typeable)

instance Exception JException

instance Exception YJException where
	toException = jToException
	fromException = jFromException

instance Exception MJException where
	toException = jToException
	fromException = jFromException

instance Exception IJException where
	toException = jToException
	fromException = jFromException
