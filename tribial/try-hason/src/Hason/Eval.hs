{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Hason.Eval where

import Control.Arrow
import Data.Char

data Reserved
	= CommentBegin | CommentEnd
	| PragmaBegin | PragmaEnd
	| LParen | RParen
	| LSqBracket | RSqBracket
	| Dot | Comma
	| DoubleColon
	| Equal
	| Import
	| Module
	| Where
	deriving Show

reservedVarTable :: [(String, Reserved)]
reservedVarTable = [
	("import", Import), ("module", Module), ("where", Where) ]

data Token
	= TReserved Reserved
	| TCon String | TVar String
	| TInt Integer
	| TString String
	| TComment String
	| TWhiteSpace
	deriving Show

isSmall :: Char -> Bool
isSmall = (||) <$> isLower <*> (== '_')

lexer :: String -> [Token]
lexer "" = []
lexer ('{' : '-' : '#' : src) =
	TReserved PragmaBegin : TComment cm : TReserved PragmaEnd : lexer src'
	where (cm, src') = pragma src
lexer ('(' : src) = TReserved LParen : lexer src
lexer (')' : src) = TReserved RParen : lexer src
lexer (',' : src) = TReserved Comma : lexer src
lexer (':' : ':' : src) = TReserved DoubleColon : lexer src
lexer ('=' : src) = TReserved Equal : lexer src
lexer (c : src)
	| isUpper c = let (t, src') = conid c src in t : lexer src'
	| isSmall c = let (t, src') = varid c src in t : lexer src'
	| isSpace c = TWhiteSpace : lexer (dropWhile isSpace src)

pragma :: String -> (String, String)
pragma "" = error "no terminate pragma"
pragma ('#' : '-' : '}' : src) = ("", src)
pragma (c : cs) = (c :) `first` pragma cs

conid :: Char -> String -> (Token, String)
conid h src = (TCon c, src')
	where
	(c, src') = (h :) `first` span isVaridTail src

varid :: Char -> String -> (Token, String)
varid h src = (
	maybe (TVar v) TReserved $ lookup v reservedVarTable,
	src' )
	where
	(v, src') = (h :) `first` span isVaridTail src

isVaridTail :: Char -> Bool
isVaridTail = (\s l d p -> s || l || d || p)
	<$> isSmall <*> isUpper <*> isDigit <*> (== '\'')
