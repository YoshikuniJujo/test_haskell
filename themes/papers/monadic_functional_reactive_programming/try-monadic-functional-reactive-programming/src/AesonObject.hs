{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module AesonObject (Value(..), Object, copyAesonValue, copyAesonObject, decodeJson) where

import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)
import Data.Scientific (Scientific)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

import qualified Data.Aeson as A

data Value
	= Object !Object | Array !Array
	| String !Text | Number !Scientific | Bool !Bool | Null
	deriving (Show, Eq, Ord)

type Object = HashMap Text Value
type Array = Vector Value

decodeJson :: ByteString -> Either String [Object]
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
