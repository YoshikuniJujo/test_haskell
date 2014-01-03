{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

module GHierarchy (
	Pointer(..), GObject(..), SomeGObject, gClass, castGObject
) where

import Foreign.Ptr
import LifeTemplate
import Template
import Language.Haskell.TH

class Pointer p where
	pointer :: p -> Ptr p
	fromPointer :: Ptr p -> p

gClass :: String -> String -> DecsQ
gClass s c = do
	d1 <- newType mkSomePointer "GObject" s c
	d2 <- sequence [
		instancePointer c,
		instancePointerSome $ "Some" ++ c
	 ]
	return $ d1 ++ d2

mkTop "Pointer" "GObject"
sequence [
--	instancePointer "SomeGObject"
 ]

instance Pointer SomeGObject where
	pointer (SomeGObject g) = castPtr $ pointer g
	fromPointer p = undefined

castGObject :: (GObject g, GObject h) => g -> Maybe h
castGObject = fromGObject . toGObject
