{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.Gobject.Hierarchy where

import Language.Haskell.TH
import Foreign.Ptr
import Control.Exception
import Control.Exception.Hierarchy
import Data.Typeable
import Data.Char

data SomeGObject = forall o . GObject o => SomeGObject o deriving Typeable

instance Show SomeGObject where
	showsPrec d (SomeGObject o) = showsPrec d o

class (Typeable o, Pointer o, Show o) => GObject o where
	toGObject :: o -> SomeGObject
	fromGObject :: SomeGObject -> Maybe o

	toGObject = SomeGObject
	fromGObject (SomeGObject o) = cast o

class Pointer a where
	pointer :: a -> (Ptr a -> IO b) -> IO b
	modifyPointer :: a -> (Ptr a -> Ptr a) -> a

instance GObject SomeGObject where
	toGObject = id
	fromGObject = Just

instance Pointer SomeGObject where
	pointer (SomeGObject o) f = pointer o $ f . castPtr
	modifyPointer (SomeGObject o) f = SomeGObject $ modifyPointer o (castPtr . f . castPtr)

gCastObject :: (GObject o1, GObject o2) => o1 -> Maybe o2
gCastObject = fromGObject . toGObject

data GObjectCastError = GObjectCastError TypeRep TypeRep deriving Show

exceptionHierarchy Nothing $ ExType ''GObjectCastError

gCastObjectIo :: (GObject o1, GObject o2) => o1 -> IO o2
gCastObjectIo o1 = maybe (throwIO $ GObjectCastError (typeOf o1) (typeRep o2)) pure o2
	where o2 = gCastObject o1

data GObjectHierarchy
	= GObjectType Name
	| GObjectNode String [GObjectHierarchy]
	deriving Show

gObjectHierarchy :: Maybe Name -> GObjectHierarchy -> DecsQ
gObjectHierarchy Nothing (GObjectType o) = (: []) <$> defInstGObject o
gObjectHierarchy (Just c) (GObjectType o) = (: []) <$> instGObject c o
gObjectHierarchy Nothing (GObjectNode (mkName -> o) os) = (\x y z -> x : y ++ concat z)
	<$> defInstGObject o <*> gObjectContainer o <*> gObjectHierarchy (Just o) `mapM` os
gObjectHierarchy (Just c) (GObjectNode (mkName -> o) os) = (\x y z -> x : y ++ concat z)
	<$> instGObject c o <*> gObjectContainer o <*> gObjectHierarchy (Just o) `mapM` os

gObjectContainer :: Name -> DecsQ
gObjectContainer oc = sequence [
	do	o <- newName "o"
		dataD (cxt []) oc [] Nothing
			[forallC [PlainTV o SpecifiedSpec] (cxt [myClassP ''GObject [varT o]])
				$ normalC oc [bangType myNotStrict (varT o)]]
			[derivClause Nothing [conT ''Typeable]],
	do	o <- newName "o"
		d <- newName "d"
		instanceD (cxt []) (conT ''Show `appT` conT oc)
			[funD 'showsPrec
				[clause [varP d, conP oc [varP o]]
					(normalB $ varE 'showsPrec `appE` varE d `appE` varE o) []]],
	-- modifyPointer (SomeGObject o) f = SomeGObject $ modifyPointer o (castPtr . f . castPtr)
	do	o <- newName "o"
		f <- newName "f"
		instanceD (cxt []) (conT ''Pointer `appT` conT oc) [
			funD 'pointer
				[clause [conP oc [varP o], varP f]
					(normalB $ varE 'pointer `appE` varE o `appE`
						infixE (Just $ varE f) (varE  '(.)) (Just $ varE 'castPtr))
					[]],
			funD 'modifyPointer
				[clause [conP oc [varP o], varP f]
					(normalB $ conE oc `appE`
						(varE 'modifyPointer `appE` varE o `appE`
							(varE 'castPtr `dot` varE f `dot` varE 'castPtr)))
					[]]
			],
	do	o <- newName "o"
		sigD toGo . forallT [PlainTV o SpecifiedSpec] (cxt [myClassP ''GObject [varT o]])
			$ varT o `arrT` conT ''SomeGObject,
	valD (varP toGo)
		(normalB $ infixE
			(Just $ varE 'toGObject) (varE '(.)) (Just $ conE oc))
		[],
	do	o <- newName "o"
		sigD fromGo . forallT [PlainTV o SpecifiedSpec] (cxt [myClassP ''GObject [varT o]])
			$ conT ''SomeGObject `arrT` (conT ''Maybe `appT` varT o),
	do	o <- newName "o"
		so <- newName "so"
		funD fromGo [clause
			[varP so]
			(normalB $ doE [
				bindS (conP oc [varP o])
					(varE 'fromGObject `appE` varE so),
				noBindS $ varE 'cast `appE` varE o])
			[]]
	]
	where
	oc' = toLower `appHead` nameBase oc
	toGo = mkName $ oc' ++ "ToGObject"
	fromGo = mkName $ oc' ++ "FromGObject"


-- infixE (Just $ varE f) (varE  '(.)) (Just $ varE 'castPtr))

infixr `dot`

dot :: ExpQ -> ExpQ -> ExpQ
dot f g = varE '(.) `appE` f `appE` g

myNotStrict :: Q Strict
myNotStrict = bang noSourceUnpackedness noSourceStrictness

infixr `arrT`
arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2

myClassP :: Name -> [Q Type] -> Q Pred
myClassP cla tys = foldl AppT (ConT cla) <$> sequence tys

defInstGObject :: Name -> DecQ
defInstGObject o = instanceD (cxt []) (conT ''GObject `appT` conT o) []

instGObject :: Name -> Name -> DecQ
instGObject (appHead toLower . nameBase -> oc) o = instanceD (cxt [])
	(conT ''GObject `appT` conT o) [
		valD (varP 'toGObject) (normalB $ varE to) [],
		valD (varP 'fromGObject) (normalB $ varE fo) [] ]
	where
	to = mkName $ oc ++ "ToGObject"
	fo = mkName $ oc ++ "FromGObject"

appHead :: (a -> a) -> [a] -> [a]
appHead _ [] = []
appHead f (h : t) = f h : t
