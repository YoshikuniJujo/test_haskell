{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CPSTest where

hellosL, hellosR :: Int -> String
hellosL n = foldl (++) "" $ n `replicate` "hello"
hellosR n = foldr (++) "" $ n `replicate` "hello"

hellosS :: Int -> ShowS
hellosS n = foldl (.) id $ n `replicate` ("hello" ++)

notNull :: ShowS -> Maybe ShowS
notNull s = case s "" of
	[] -> Nothing
	_ -> Just s

toS :: String -> ShowS
toS = (++)

fromS :: ShowS -> String
fromS = ($ "")

checkCat :: ShowS -> Bool
checkCat s = case s "" of
	'c' : 'a' : 't' : _ -> True
	_ -> False

addCat :: ShowS -> ShowS
addCat s
	| checkCat s = s . ("cat" ++)
	| otherwise = s

testAddCat n = last $ ((!! n) $ iterate addCat ("cat" ++)) ""

type Strings = [String]

checkHead :: String -> Strings -> Bool
checkHead "" _ = True
checkHead cs0 ([] : ss) = checkHead cs0 ss
checkHead (c0 : cs0) ((c : cs) : ss)
	| c0 == c = checkHead cs0 (cs : ss)
	| otherwise = False

checkCat' :: Strings -> Bool
checkCat' = checkHead "cat"

addCat' :: Strings -> Strings
addCat' s
	| checkCat' s = s ++ ["cat"]
	| otherwise = s

testAddCat' n = last $ concat $ (!! n) $ iterate addCat' ["cat"]
