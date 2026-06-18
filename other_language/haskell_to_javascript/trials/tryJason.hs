{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Array qualified as V
import Data.Map qualified as M
import Data.Text qualified as T

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object

main :: IO ()
main = do
	JS.Value.consoleLog $ JS.Value.toV (123 :: Int)
	o <- JS.Object.new
	JS.Object.set o "foobar" "baz"
	JS.Object.consoleLog o

data Value
	= Object Object

type Object = KeyMap Value

type KeyMap a = M.Map T.Text a
