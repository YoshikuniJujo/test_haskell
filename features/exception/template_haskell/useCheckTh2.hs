{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, ExistentialQuantification #-}

import Control.Exception
import Data.Tree
import Data.Typeable

import CheckTh

newtype GhostError = GhostError String deriving (Typeable, Show)
newtype MyException = MyException String deriving (Typeable, Show)
newtype ManError = ManError String deriving (Typeable, Show)
newtype WomanError = WomanError String deriving (Typeable, Show)
newtype WolfmanError = WolfmanError String deriving (Typeable, Show)
newtype MermaidError = MermaidError String deriving (Typeable, Show)

exceptionHierarchy2 Nothing (ExType ''MyException)
exceptionHierarchy2 Nothing (
	ExNode "HumanError" [
		ExType ''ManError, 
		ExNode "DemihumanError" [],
		ExNode "NomanError" [
			ExType ''GhostError ] ] )

exceptionHierarchy2 (Just ''HumanError) (ExType ''WomanError)
exceptionHierarchy2 (Just ''DemihumanError) (ExType ''WolfmanError)

instance Exception MermaidError where
	toException = demihumanErrorToException
	fromException = demihumanErrorFromException
