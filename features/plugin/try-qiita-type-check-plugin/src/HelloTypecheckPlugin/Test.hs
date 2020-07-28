{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=HelloTypecheckPlugin.Plugin #-}

module HelloTypecheckPlugin.Test where

some :: Int -> Bool
some = id

{-
-- import Data.Type.Bool

type family If (b :: Bool) t e

type instance If 'True a b = a

some :: Int -> If 'True Int Char
some = id

-- other :: Int -> If 
-}
