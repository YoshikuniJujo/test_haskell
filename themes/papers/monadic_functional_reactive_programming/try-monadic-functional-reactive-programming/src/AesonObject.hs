{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module AesonObject (Value(..), Object, copyAesonValue, copyAesonObject, decodeJson) where

import Data.Vector
import Data.HashMap.Strict
import Data.Scientific

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Aeson as A

data Value
	= Object !Object | Array !Array
	| String !T.Text | Number !Scientific | Bool !Bool | Null
	deriving (Show, Eq, Ord)

type Object = HashMap T.Text Value
type Array = Vector Value

decodeJson :: LBS.ByteString -> Either String [Object]
decodeJson = ((copyAesonObject <$>) <$>) . A.eitherDecode

copyAesonValue :: A.Value -> Value
copyAesonValue (A.Object o) = Object $ copyAesonObject o
copyAesonValue (A.Array a) = Array $ copyAesonArray a
copyAesonValue (A.String s) = String s
copyAesonValue (A.Number n) = Number n
copyAesonValue (A.Bool b) = Bool b
copyAesonValue A.Null = Null

copyAesonObject :: A.Object -> Object
copyAesonObject = (copyAesonValue <$>)

copyAesonArray :: A.Array -> Array
copyAesonArray = (copyAesonValue <$>)
