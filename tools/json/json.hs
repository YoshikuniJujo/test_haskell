import Prelude hiding (null)
import qualified Prelude

import Data.Char

import Parse

data Json
	= Number Integer
	| String String
	| Bool Bool
	| Array [Json]
	| Object [(String, Json)]
	| Null
	deriving Show

decode :: String -> Maybe Json
decode s = case filter (Prelude.null . snd) $ json s of
	(j, "") : _ -> Just j
	_ -> Nothing

json :: Parse Json
json = number `build` Number
	`alt` string `build` String
	`alt` bool `build` Bool
	`alt` array `build` Array
	`alt` object `build` Object
	`alt` null `build` const Null

number :: Parse Integer
number = list1 (spot isDigit) `build` read

string :: Parse String
string = char '"' *> list (spot (/= '"')) >* char '"'

bool, false, true :: Parse Bool
false = foldr (*>) (succeed ()) (map char "false") `build` const False
true = foldr (*>) (succeed ()) (map char "true") `build` const True
bool = false `alt` true

null :: Parse ()
null = foldr (*>) (succeed ()) (map char "null") `build` const ()

array :: Parse [Json]
array = char '[' >*> char ']' `build` const [] `alt`
	char '[' *> json >*> list (char ',' *> json) >* char ']' `build` uncurry (:)

object :: Parse [(String, Json)]
object = char '{' *> char '}' `build` const [] `alt`
	char '{' *> member >*> list (char ',' *> member) >* char '}'
		`build` uncurry (:)

member :: Parse (String, Json)
member = string >*> char ':' *> json
