{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Functor.ProductIsomorphic
import Database.Relational

hello :: Relation () (Int, String)
hello = relation $ return (value 0 >< value "Hello")

world :: Relation () (Int, String)
world = relation $ return (value 0 >< value "World")

helloWorld :: Relation () (Int, String, String)
helloWorld = relation $ do
	h <- query hello
	w <- query world
	on $ h ! fst' .=. w !fst'
	return $ (,,) |$| h ! fst' |*| h ! snd' |*| w ! snd'

main :: IO ()
main = putStrLn $ show helloWorld ++ ";"
