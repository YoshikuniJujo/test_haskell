{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Complete where

import Language.Haskell.TH

data Foo = Foo_ Int deriving Show

pattern Foo :: Int -> Foo
pattern Foo n <- Foo_ n

foo :: DecsQ
foo = [d| {-# COMPLETE Foo #-} |]
