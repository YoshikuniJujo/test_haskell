{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.Gobject.Hierarchy where

import Language.Haskell.TH
import Foreign.Ptr
import Data.Typeable

data SomeGObject = forall o . GObject o => SomeGObject o deriving Typeable

instance Show SomeGObject where
	showsPrec d (SomeGObject o) = showsPrec d o

class (Typeable o, Show o) => GObject o where
	toGObject :: o -> SomeGObject
	fromGObject :: SomeGObject -> Maybe o
	pointer :: o -> Ptr o

	toGObject = SomeGObject
	fromGObject (SomeGObject o) = cast o

instance GObject SomeGObject where
	toGObject = id
	fromGObject = Just
	pointer (SomeGObject o) = castPtr $ pointer o

data GObjectHierarchy
	= GObjectNode String [GObjectHierarchy]
	| GObjectType Name
	deriving Show

defInstGObject :: Name -> DecQ
defInstGObject o = instanceD (cxt []) (conT ''GObject `appT` conT o) []

appHead :: (a -> a) -> [a] -> [a]
appHead _ [] = []
appHead f (h : t) = f h : t
