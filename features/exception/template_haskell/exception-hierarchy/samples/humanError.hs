{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, ExistentialQuantification #-}

import Control.Exception
import Data.Typeable

import Control.Exception.Hierarchy

newtype GhostError = GhostError String deriving (Typeable, Show)
newtype MyException = MyException String deriving (Typeable, Show)
newtype ManError = ManError String deriving (Typeable, Show)
newtype WomanError = WomanError String deriving (Typeable, Show)
newtype WolfmanError = WolfmanError String deriving (Typeable, Show)
newtype MermaidError = MermaidError String deriving (Typeable, Show)

exceptionHierarchy Nothing (ExType ''MyException)
exceptionHierarchy Nothing (
	ExNode "HumanError" [
		ExType ''ManError, 
		ExNode "DemihumanError" [],
		ExNode "NomanError" [
			ExType ''GhostError ] ] )

exceptionHierarchy (Just ''HumanError) (ExType ''WomanError)
exceptionHierarchy (Just ''DemihumanError) (ExType ''WolfmanError)

instance Exception MermaidError where
	toException = demihumanErrorToException
	fromException = demihumanErrorFromException
