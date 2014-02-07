{-# LANGUAGE QuasiQuotes, TypeFamilies, PackageImports #-}

module Parser (
	parseMrd
) where

import Control.Arrow
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Error
import Data.Maybe
import Data.Char
import Text.Papillon

import Text

parseMrd :: String -> Maybe [Text]
parseMrd src = case flip runState (0, [- 1]) $ runErrorT $ markdown $ parse src of
	(Right (r, _), _) -> Just r
	_ -> Nothing

clear :: State (Int, [Int]) Bool
clear = put (0, [- 1]) >> return True

reset :: State (Int, [Int]) Bool
reset = modify (first $ const 0) >> return True

count :: State (Int, [Int]) ()
count = modify $ first (+ 1)

deeper :: State (Int, [Int]) Bool
deeper = do
	(n, n0 : ns) <- get
	if n > n0 then put (n, n : n0 : ns) >> return True else return False

same :: State (Int, [Int]) Bool
same = do
	(n, n0 : _) <- get
	return $ n == n0

shallow :: State (Int, [Int]) Bool
shallow = do
	(n, n0 : ns) <- get
	if n < n0 then put (n, ns) >> return True else return False

[papillon|

monad: State (Int, [Int])

markdown :: [Text]
	= md:(m:markdown1 _:dmmy[clear] { return m })*		{ return md }

markdown1 :: Text
	= h:header				{ return h }
	/ c:code				{ return $ Code c }
	/ l:list				{ return $ List l }
	/ p:paras				{ return $ Paras p }

header :: Text
	= n:sharps _:<isSpace>* l:line '\n'+	{ return $ Header n l }
	/ l:line '\n' _:equals '\n'+		{ return $ Header 1 l }
	/ l:line '\n' _:hyphens '\n'+		{ return $ Header 2 l }

sharps :: Int
	= '#' n:sharps				{ return $ n + 1 }
	/ '#'					{ return 1 }

equals :: ()
	= '=' _:equals
	/ '='

hyphens :: ()
	= '-' _:hyphens
	/ '-'

line :: String
	= l:<(`notElem` "#\n")>+		{ return l }

code :: String
	= l:fourSpacesLine c:code		{ return $ l ++ c }
	/ l:fourSpacesLine			{ return l }

fourSpacesLine :: String
	= _:fourSpaces l:line '\n'+		{ return $ l ++ "\n" }

fourSpaces :: ()
	= ' ' ' ' ' ' ' '

list :: List = _:cnt _:dmmy[deeper] l:list1 ls:list1'* _:shllw	{ return $ l : ls }
--	= _:cnt _:dmmy[deeper] l:list1 -- ls:list1'* _:shllw
--						{ return $ l : ls }
{-
	= ls:(_:listHead ' ' l:line '\n' s:subList { return $ BulItem l s })+
						{ return ls }
	/ ls:(_:nListHead ' ' l:line '\n' s:subList { return $ OrdItem l s })+
						{ return ls }
						-}

cnt :: () = _:dmmy[reset] _:(' ' { count })*

list1' :: List1
	= _:cnt _:dmmy[same] l:list1		{ return l }

list1 :: List1
	= _:listHead ' ' l:line '\n' ls:list?
		{ return $ BulItem l $ fromMaybe [] ls }
	/ _:nListHead ' ' l:line '\n' ls:list?
		{ return $ OrdItem l $ fromMaybe [] ls }

subList :: List
	= ls:subList1*				{ return ls }

subList1 :: List1
	= _:fourSpaces _:listHead ' ' l:line '\n'	{ return $ BulItem l [] }
	/ _:fourSpaces _:nListHead ' ' l:line '\n'	{ return $ OrdItem l [] }

listHead :: ()
	= '*' / '-' / '+'

nListHead :: ()
	= _:<isDigit>+ '.'

paras :: [String]
	= ps:para+				{ return ps }

para :: String
	= ls:(!_:header !_:fourSpaces l:line '\n' { return l })+ _:('\n' / !_ / !_:para)	{ return $ concat ls }

shllw :: ()
	= _:dmmy[shallow]
	/ !_
	/ !_:list

dmmy :: () =

|]
