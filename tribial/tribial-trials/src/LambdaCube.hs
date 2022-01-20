{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LambdaCube where

class TypeName a where typeName :: String

instance TypeName Int where typeName = "Int"
instance TypeName Double where typeName = "Double"
instance TypeName Bool where typeName = "Bool"

typeNameDouble :: String
typeNameDouble = typeName @Double
