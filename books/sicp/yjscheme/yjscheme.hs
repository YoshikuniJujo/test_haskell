{-# LANGUAGE QuasiQuotes, PackageImports #-}

module Main where

import Text.Papillon
import Data.Char
import System.IO
import Control.Applicative
import "monads-tf" Control.Monad.State

main :: IO ()
main = do
	flip runStateT initEnv $ do
		doWhile_ $ do
			ln <- lift $ do
				putStr "yjscheme> "
				hFlush stdout
				getLine
			case ln of
				":q" -> return False
				_ -> do	case prs ln of
						Just p -> do
							r <- eval p
							lift $ printObj r
						Nothing -> lift $ putStrLn 
							"parse error"
					return True
	return ()

type Env = [(String, Object)]

printObj :: Maybe Object -> IO ()
printObj (Just o) = putStrLn $ showObj o
printObj _ = putStrLn "can't eval"

initEnv :: Env
initEnv = [
	("+", OSubr "+" undefined),
	("-", OSubr "-" undefined),
	("*", OSubr "*" undefined),
	("/", OSubr "/" undefined)
 ]

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ act = do
	b <- act
	if b then doWhile_ act else return ()

eval :: Object -> StateT Env IO (Maybe Object)
eval i@(OInt _) = return $ return i
eval (OVar v) = gets $ lookup v
{-
eval (OList os) = do
	f : args <- mapM eval os
	case f of
		OSubr _ s -> Just <$> s args
		_ -> Nothing
		-}

prs :: String -> Maybe Object
prs src = case runError $ scm $ parse src of
	Right (r, _) -> Just r
	_ -> Nothing

data Object
	= OInt Integer
	| OVar String
	| OList [Object]
	| OSubr String ([Object] -> StateT Env IO Object)

showObj :: Object -> String
showObj (OInt i) = show i
showObj (OVar v) = v
showObj (OSubr n _) = "#<subr " ++ n ++ ">"

data Tkn
	= TIntL Integer
	| TVar String
	| TOParen
	| TCParen

isVar :: Char -> Bool
isVar = (||) <$> isAlpha <*> (`elem` "+-*/")

[papillon|

scm :: Object
	= o:obj _:spaces !_	{ o }

obj :: Object
	= (TIntL i):lx		{ OInt i }
	/ (TVar v):lx		{ OVar v }
	/ TOParen:lx os:obj* TCParen:lx
				{ OList os }

lx :: Tkn
	= s:spaces w:word	{ w }

word :: Tkn
	= ds:<isDigit>+		{ TIntL $ read ds }
	/ v:<isVar>+		{ TVar v }
	/ '('			{ TOParen }
	/ ')'			{ TCParen }

spaces = _:<isSpace>*

|]
