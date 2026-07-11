module Main where

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Node qualified as JS.Node
import GHC.JS.Value.Window qualified as JS.Window
import GHC.JS.Value.Document qualified as JS.Document
import GHC.JS.Value.CharacterData.Text qualified as JS.Text

main :: IO ()
main = do
	putStrLn "foobar"
	let	doc = JS.Window.document JS.Window.w
	txt <- JS.Text.new "Hello"
	JS.Value.consoleLog txt
	JS.Node.appendChild (JS.Node.toN $ JS.Document.body doc) (JS.Node.toN txt)
