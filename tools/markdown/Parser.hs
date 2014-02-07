{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

module Parser (
	parseMrd
) where

import Data.Char
import Text.Papillon

import Text

parseMrd :: String -> Maybe [Text]
parseMrd src = case runError $ markdown $ parse src of
	Right (r, _) -> Just r
	_ -> Nothing

[papillon|

markdown :: [Text]
	= md:markdown1*				{ md }

markdown1 :: Text
	= h:header				{ h }
	/ c:code				{ Code c }
	/ l:list				{ List l }
	/ p:paras				{ Paras p }

header :: Text
	= n:sharps _:<isSpace>* l:line '\n'	{ Header n l }
	/ l:line '\n' _:equals '\n'		{ Header 1 l }
	/ l:line '\n' _:hyphens '\n'		{ Header 2 l }

sharps :: Int
	= '#' n:sharps				{ n + 1 }
	/ '#'					{ 1 }

equals :: ()
	= '=' _:equals
	/ '='

hyphens :: ()
	= '-' _:hyphens
	/ '-'

line :: String
	= l:<(`notElem` "#\n")>+		{ l }

code :: String
	= l:fourSpacesLine c:code		{ l ++ c }
	/ l:fourSpacesLine			{ l }

fourSpacesLine :: String
	= _:fourSpaces l:line '\n'		{ l ++ "\n" }

fourSpaces :: ()
	= ' ' ' ' ' ' ' '

list :: List
	= ls:(_:listHead ' ' l:line '\n' s:subList { BulItem l s })+
						{ ls }
	/ ls:(_:nListHead ' ' l:line '\n' s:subList { OrdItem l s })+
						{ ls }

subList :: List
	= ls:subList1*				{ ls }

subList1 :: List1
	= _:fourSpaces _:listHead ' ' l:line '\n'	{ BulItem l [] }
	/ _:fourSpaces _:nListHead ' ' l:line '\n'	{ OrdItem l [] }

listHead :: ()
	= '*' / '-' / '+'

nListHead :: ()
	= _:<isDigit>+ '.'

paras :: [String]
	= ps:para+				{ ps }

para :: String
	= ls:(!_:fourSpaces l:line '\n' { l })+ _:('\n' / !_ / !_:para)	{ concat ls }

|]
