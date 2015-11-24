{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, ExistentialQuantification #-}

import Control.Exception
import Data.Tree
import Data.Typeable

import CheckTh

newtype GhostError = GhostError String deriving (Typeable, Show)
newtype MyException = MyException String deriving (Typeable, Show)
newtype ManError = ManError String deriving (Typeable, Show)

exceptionTree Nothing (Node ''MyException [])
exceptionTree Nothing (Node (mkName "HumanError") [
	Node ''ManError [], 
	Node (mkName "NomanError") [
		Node ''GhostError [] ] ])

{-
myException

humanError

manError

nomanError

ghostError
-}
