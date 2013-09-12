{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

module Parse (
	readData
) where

import Text.Papillon
import Data.Char
import Data.Maybe

readData :: FilePath -> IO [(String, Either String [String])]
readData fp = do
	ret <- prs <$> readFile fp
	case ret of
		Just r -> return r
		Nothing -> error "parse error"

prs :: String -> Maybe [(String, Either String [String])]
prs src = case runError $ defs $ parse src of
	Right (r, _) -> Just r
	Left _ -> Nothing

[papillon|

defs :: [(String, Either String [String])]
	= ds:def* !_			{ ds }

def :: (String, Either String [String])
	= t:title ':' ' ' v:value '\n'+	{ (t, v) }

title :: String
	= s:<isAlphaNum>+	{ s }

value :: Either String [String]
	= !'.' s:(c:[c `notElem` "\\\n"] { Just c } / '\\' '\n' { Nothing })+
				{ Left $ catMaybes s }
	/ '\\' 'l' 'i' 's' 't' '\n' ms:(!'.' s:<(`notElem` "\n")>+ '\n' { s })*
		'.'
				{ Right ms }

|]
