{-# OPTIONS_GHC -Wname-shadowing -fwarn-unused-matches -fwarn-unused-binds #-}

module Shadow where

func1 x y = do x + y

main :: IO ()
main = do
	let a = 1
	print ("a=" ++ (show a))

	let data1 = func1 a 5
	let a = data1
	print ("a=" ++ (show a))

	let data1 = func1 a (- 3)
	let a = data1
	print ("a=" ++ (show a))

foo :: Int
foo = let foo = 8 in foo

bar x = 123

baz = \x -> 456

hoge = \_x -> 456
