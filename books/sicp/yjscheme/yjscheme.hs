{-# LANGUAGE QuasiQuotes #-}

module Main where

import Text.Papillon
import Data.Char
import System.IO
import Control.Applicative

main :: IO ()
main = do
	doWhile_ $ do
		putStr "yjscheme> "
		hFlush stdout
		ln <- getLine
		case ln of
			":q" -> return False
			_ -> do	putStrLn $ maybe "parse error" showObj $
					eval <$> prs ln
				return True

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ act = do
	b <- act
	if b then doWhile_ act else return ()

eval :: Object -> Object
eval i@(IntO _) = i

prs :: String -> Maybe Object
prs src = case runError $ scm $ parse src of
	Right (r, _) -> Just r
	_ -> Nothing

data Object
	= IntO Integer
	deriving Show

showObj (IntO i) = show i

data Tkn
	= IntL Integer
	| Other

[papillon|

scm :: Object
	= o:obj _:spaces !_	{ o }

obj :: Object
	= (IntL i):lx		{ IntO i }

lx :: Tkn
	= s:spaces w:word	{ w }

word :: Tkn
	= ds:<isDigit>+		{ IntL $ read ds }

spaces = _:<isSpace>*

|]
