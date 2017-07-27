{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import Data.Char
import System.Environment
import Text.Papillon

main :: IO ()
main = do
	arg : _ <- getArgs
	case runError $ message $ parse arg of
		Right (r, _) -> print r
		Left _ -> putStrLn "parse error"

data Greeting = Hello | GoodBye deriving Show

data Name = Yoshikuni | Manami | Itsuki deriving Show

[papillon|

message :: (Greeting, Name)
	= g:greeting ',' ' ' n:name	{ (g, n) }

greeting :: Greeting
	= "Hello":( c:[isAlphaNum c] { c } )*		{ Hello }
	/ "Good-bye":( c:[isAlphaNum c] { c } )*	{ GoodBye }

name :: Name
	= "Yoshikuni":( c:[isAlphaNum c] { c } )*	{ Yoshikuni }
	/ "Manami":( c { c } )*		{ Manami }
	/ "Itsuki":( c { c } )*		{ Itsuki }

|]
