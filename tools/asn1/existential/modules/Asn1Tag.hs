{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}

module Asn1Tag (
	Asn1Tag(..), TagClass(..), DataClass(..)) where

data Asn1Tag
	= Asn1Tag TagClass DataClass Integer
	deriving (Show, Eq)

data TagClass
	= Universal
	| Application
	| ContextSpecific
	| Private
	deriving (Show, Eq, Enum)

data DataClass
	= Primitive
	| Constructed
	deriving (Show, Eq, Enum)
