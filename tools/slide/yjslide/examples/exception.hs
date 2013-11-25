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

data HumanError = forall e . Exception e => HumanError e deriving Typeable

instance Show HumanError where
	show (HumanError e) = show e

instance Exception HumanError

humanErrorToException :: Exception e => e -> SomeException
humanErrorToException = toException . HumanError

humanErrorFromException :: Exception e => SomeException -> Maybe e
humanErrorFromException se = do
	HumanError e <- fromException se
	cast e

data ManError = ManError deriving (Show, Typeable)

instance Exception ManError where
	toException = humanErrorToException
	fromException = humanErrorFromException
