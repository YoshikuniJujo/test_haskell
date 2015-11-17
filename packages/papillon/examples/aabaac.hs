{-# LANGUAGE QuasiQuotes #-}

import Text.Papillon

prs str = case runError . a $ parse str of
	Right (r, _) -> Just r
	Left _ -> Nothing

[papillon|

a :: String
	= 'a' s:a 'b'	{ 'a' : s ++ "b" }
	/ 'a' s:a 'c'	{ 'a' : s ++ "c" }
	/		{ "" }

|]
