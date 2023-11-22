{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.Gobject.TryHierarchy where

import Foreign.Ptr
import System.Gobject.Hierarchy

data Foo = Foo (Ptr Foo) deriving Show
instance Pointer Foo where
	pointer (Foo p) = ($ p)
gObjectHierarchy Nothing (GObjectType ''Foo)

data Bar = Bar (Ptr Bar) deriving Show
instance Pointer Bar where
	pointer (Bar p) = ($ p)
gObjectHierarchy Nothing $ GObjectNode "Baz" [GObjectType ''Bar]

{-

(: []) <$> defInstGObject ''Foo

gObjectContainer $ mkName "Baz"

instance GObject Baz


(: []) <$> instGObject ''Baz ''Bar
-}
