{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

import Control.Exception
import Data.Typeable

data HumanError = forall e . Exception e => HumanError e deriving Typeable

instance Show HumanError where show (HumanError e) = show e

instance Exception HumanError

humanErrorToException :: Exception e => e -> SomeException
humanErrorToException = toException . HumanError

humanErrorFromException :: Exception e => SomeException -> Maybe e
humanErrorFromException se = do
	HumanError e <- fromException se
	cast e

data ManError = ManError deriving (Typeable, Show)

instance Exception ManError where
	toException = humanErrorToException
	fromException = humanErrorFromException

data WomanError = WomanError deriving (Typeable, Show)

instance Exception WomanError where
	toException = humanErrorToException
	fromException = humanErrorFromException

data NomanError = forall e . Exception e => NomanError e deriving Typeable

instance Show NomanError where show (NomanError e) = show e

instance Exception NomanError where
	toException = humanErrorToException
	fromException = humanErrorFromException

nomanErrorToException :: Exception e => e -> SomeException
nomanErrorToException = toException . NomanError

nomanErrorFromException :: Exception e => SomeException -> Maybe e
nomanErrorFromException se = do
	SomeException e <- fromException se
	cast e

data GhostError = GhostError deriving (Typeable, Show)

instance Exception GhostError where
	toException = nomanErrorToException
	fromException = nomanErrorFromException
