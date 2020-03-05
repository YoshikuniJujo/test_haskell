{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Followbox.AesonObject (Value(..), Object, Array, decodeJson) where

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

copyAesonObject :: A.Object -> Object
copyAesonObject = (copyAesonValue <$>)

copyAesonArray :: A.Array -> Array
copyAesonArray = (copyAesonValue <$>)

copyAesonValue :: A.Value -> Value
copyAesonValue = \case
	A.Object o -> Object $ copyAesonObject o
	A.Array a -> Array $ copyAesonArray a
	A.String s -> String s
	A.Number n -> Number n
	A.Bool b -> Bool b
	A.Null -> Null
