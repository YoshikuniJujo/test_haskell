{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Hason.Eval where

import Control.Arrow
import Data.Char

data Reserved
	= CommentBegin | CommentEnd
	| PragmaBegin | PragmaEnd
	| LParen | RParen
	| LCurly | RCurly | SemiColon
	| LSqBracket | RSqBracket
	| Dot | Comma
	| DoubleColon
	| Equal
	| Import
	| Module
	| Where
	| Let | Do | Of
	deriving (Show, Eq)

reservedVarTable :: [(String, Reserved)]
reservedVarTable = [
	("import", Import), ("module", Module), ("where", Where),
	("let", Let), ("do", Do), ("of", Of) ]

data Token
	= TReserved Reserved
	| TCon String | TVar String
	| TInt Integer
	| TString String
	| TComment [String]
	| TWhiteSpace
	deriving Show

data Offset
	= OCurly Int
	| OAngle Int
	| OToken Token
	deriving Show

isOffsetBeginer :: Token -> Bool
isOffsetBeginer (TReserved r) = r `elem` [Where, Let, Do, Of]
isOffsetBeginer _ = False

isSmall :: Char -> Bool
isSmall = (||) <$> isLower <*> (== '_')

lexer :: Int -> String -> [Offset]
lexer _ "" = []
lexer i ('{' : '-' : '#' : src) =
	OToken (TReserved PragmaBegin) : OToken (TComment cm) : OToken (TReserved PragmaEnd) : lexer i' src'
	where (i', (cm, src')) = pragma i src
lexer i ('(' : src) = OToken (TReserved LParen) : lexer (i + 1) src
lexer i (')' : src) = OToken (TReserved RParen) : lexer (i + 1) src
lexer i ('[' : src) = OToken (TReserved LSqBracket) : lexer (i + 1) src
lexer i (']' : src) = OToken (TReserved RSqBracket) : lexer (i + 1) src
lexer i ('{' : src) = OToken (TReserved LCurly) : lexer (i + 1) src
lexer i ('}' : src) = OToken (TReserved RCurly) : lexer (i + 1) src
lexer i (';' : src) = OToken (TReserved SemiColon) : lexer (i + 1) src
lexer i (',' : src) = OToken (TReserved Comma) : lexer (i + 1) src
lexer i (':' : ':' : src) = OToken (TReserved DoubleColon) : lexer (i + 2) src
lexer i ('=' : src) = OToken (TReserved Equal) : lexer (i + 1) src
lexer i ('"' : src) = let (i', (str, src')) = string (i + 1) src in OToken (TString str) : lexer i' src'
lexer i sa@(c : src)
	| isSpace c =
		let (i', (nl, src')) = spaces i False sa in
		if nl
		then OAngle i' : lexer i' src'
		else OToken TWhiteSpace : lexer i' src'
	| isUpper c = let (i', (t, src')) = conid i c src in OToken t : lexer i' src'
	| isSmall c = let (i', (t, src')) = varid i c src in
		if isOffsetBeginer t
		then OToken t : lexerBeginOffset i' src'
		else OToken t : lexer i' src'
	| isDigit c = let (d, src') = span isDigit sa in OToken (TInt (read d)) : lexer (i + length d) src'

lexerBeginOffset :: Int -> String -> [Offset]
lexerBeginOffset _ "" = [OCurly 0]
lexerBeginOffset i ca@(c : _)
	| isSpace c = lexerBeginOffset i' src'
		where (i', (_, src')) = spaces i False ca
lexerBeginOffset i ('{' : src) = OToken (TReserved LCurly) : lexer (i + 1) src
lexerBeginOffset i src = OCurly i : lexer i src

pragma :: Int -> String -> (Int, ([String], String))
pragma _ "" = error "no terminate pragma"
pragma i ('#' : '-' : '}' : src) = (i + 3, ([], src))
pragma i ca@(c : _)
	| isSpace c = let (i', (_, src)) = spaces i False ca in pragma i' src
	| otherwise = let (w, src) = span (not . isSpace) ca in
		((w :) `first`) `second` pragma (i + length w) src

conid :: Int -> Char -> String -> (Int, (Token, String))
conid i h src = (i + length c, (TCon c, src'))
	where
	(c, src') = (h :) `first` span isVaridTail src

varid :: Int -> Char -> String -> (Int, (Token, String))
varid i h src = (,) (i + length v) (
	maybe (TVar v) TReserved $ lookup v reservedVarTable,
	src' )
	where
	(v, src') = (h :) `first` span isVaridTail src

isVaridTail :: Char -> Bool
isVaridTail = (\s l d p -> s || l || d || p)
	<$> isSmall <*> isUpper <*> isDigit <*> (== '\'')

spaces :: Int -> Bool -> String -> (Int, (Bool, String))
spaces i nl (' ' : src) = spaces (i + 1) nl src
spaces i nl ('\t' : src) = spaces (((i - 1) `div` 8 + 1) * 8 + 1) nl src
spaces _ _ ('\n' : src) = spaces 1 True src
spaces _ _ ('\r' : '\n' : src) = spaces 1 True src
spaces i nl src = (i, (nl, src))

string :: Int -> String -> (Int, (String, String))
string _ "" = error "no close \""
string i ('"' : src) = (i + 1, ("", src))
string i (c : src) = ((c :) `first`) `second` string (i + 1) src

largeL :: [Offset] -> [Int] -> [Token]
largeL ta@(OAngle n : ts) ma@(m : ms)
	| m == n = TReserved SemiColon : largeL ts ma
	| n < m = largeL ta ms
largeL (OAngle _ : ts) ms = largeL ts ms
largeL (OCurly n : ts) ma@(m : _)
	| n > m = TReserved LCurly : largeL ts (n : ma)
largeL (OCurly n : ts) []
	| n > 0 = TReserved LCurly : largeL ts [n]
largeL (OCurly n : ts) ms =
	TReserved LCurly : TReserved RCurly : largeL (OAngle n : ts) ms
largeL (OToken (TReserved RCurly) : _) _ = error "parse error"
largeL (OToken (TReserved LCurly) : ts) ms =
	TReserved LCurly : largeL ts (0 : ms)
-- largeL ta@(t : _) (m : ms)
--	| m /= 0 && doesParseError t = TReserved RCurly : largeL ta ms
largeL (OToken t : ts) ms = t : largeL ts ms
largeL [] [] = []
largeL [] (m : ms) | m /= 0 = TReserved RCurly : largeL [] ms
