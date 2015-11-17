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

json :: Parse Char Json
json = number `build` Number
	`alt` string `build` String
	`alt` bool `build` Bool
	`alt` array `build` Array
	`alt` object `build` Object
	`alt` null `build` const Null

number :: Parse Char Integer
number = list1 (spot isDigit) `build` read

string :: Parse Char String
string = char '"' *> list (spot (/= '"')) >* char '"'

bool, false, true :: Parse Char Bool
bool = false `alt` true
false = foldr ((*>) . char) (succeed ()) "false" `build` const False
true = foldr ((*>) . char) (succeed ()) "true" `build` const True

null :: Parse Char ()
null = foldr ((*>) . char) (succeed ()) "null" `build` const ()

array :: Parse Char [Json]
array = char '[' >*> char ']' `build` const [] `alt`
	char '[' *> json >*> list (char ',' *> json) >* char ']' `build` uncurry (:)

object :: Parse Char [(String, Json)]
object = char '{' *> char '}' `build` const [] `alt`
	char '{' *> member >*> list (char ',' *> member) >* char '}'
		`build` uncurry (:)

member :: Parse Char (String, Json)
member = string >*> char ':' *> json
