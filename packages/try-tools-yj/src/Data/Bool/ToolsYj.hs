{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Bool.ToolsYj (onlyIf, errorIf, errorIfNot) where

import Data.Bool

onlyIf :: (a -> Bool) -> a -> Maybe a
onlyIf p x = bool Nothing (Just x) (p x)

errorIf :: String -> Bool -> IO ()
errorIf msg = bool (pure ()) (error msg)

errorIfNot :: String -> Bool -> IO ()
errorIfNot msg = bool (error msg) (pure ())
