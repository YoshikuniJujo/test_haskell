{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module WithType where

import Unsafe.Coerce

data WithType = forall x . WithType String x

fromInt :: Int -> WithType
fromInt = WithType "Int"

fromBool :: Bool -> WithType
fromBool = WithType "Bool"

fromChar :: Char -> WithType
fromChar = WithType "Char"

toInt :: WithType -> Maybe Int
toInt (WithType "Int" x) = Just $ unsafeCoerce x
toInt _ = Nothing

toBool :: WithType -> Maybe Bool
toBool (WithType "Bool" x) = Just $ unsafeCoerce x
toBool _ = Nothing

toChar :: WithType -> Maybe Char
toChar (WithType "Char" x) = Just $ unsafeCoerce x
toChar _ = Nothing
