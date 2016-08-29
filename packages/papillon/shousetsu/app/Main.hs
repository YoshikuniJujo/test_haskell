{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

module Main where

import Lib

import Text.Papillon

data STree
	= Raw String
	| Dash
	| Bold [STree]
	| Dotted [STree]
	| Rubi [STree] String
	deriving Show

main :: IO ()
main = interact trans

trans :: String -> String
trans src = case runError . shousetsu $ parse src of
	Right (r, _) -> toPseudHtml =<< r
	Left _ -> "parse error"

gather :: [Either Char STree] -> [STree]
gather (Left c : src) = case gather src of
	Raw s : sts -> Raw (c : s) : sts
	sts -> Raw [c] : sts
gather (Right st : src) = st : gather src
gather [] = []

toPseudHtml :: STree -> String
toPseudHtml (Raw s) = s
toPseudHtml Dash = "<dash/>"
toPseudHtml (Bold s) = "<bold>" ++ (toPseudHtml =<< s) ++ "</bold>"
toPseudHtml (Dotted s) = "<dotted>" ++ (toPseudHtml =<< s) ++ "</dotted>"
toPseudHtml (Rubi s r) =
	"<r>" ++ (toPseudHtml =<< s) ++ "<rubi>" ++ r ++ "</rubi></r>"

[papillon|

tok :: Either Char STree
	= '<' '-' '-'				{ Right Dash }
	/ '<' s:sst '!'				{ Right $ Bold s }
	/ '<' s:sst '|'				{ Right $ Dotted s }
	/ '<' s:sst '(' str:(c:<(/=')')> {c})* ')'
						{ Right $ Rubi s str }
	/ c:<(`notElem` "<(!|")>		{ Left c }

token :: Either Char STree
	= t:tok					{ t }
	/ '!'					{ Left '!' }
	/ '|'					{ Left '|' }
	/ '('					{ Left '(' }

sst :: [STree]
	= s:(t:tok {t})+			{ gather s }
	
shousetsu :: [STree]
	= s:(t:token {t})*			{ gather s }

|]
