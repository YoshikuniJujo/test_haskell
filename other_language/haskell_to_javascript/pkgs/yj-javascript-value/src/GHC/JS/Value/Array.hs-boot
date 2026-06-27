module GHC.JS.Value.Array where

import GHC.JS.Prim (JSVal)

newtype A = A { unA :: JSVal }

new :: IO A

js_push :: JSVal -> JSVal -> IO ()
