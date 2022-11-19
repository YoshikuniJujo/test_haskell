{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Hason (Hason(..), ppr) where

import Prelude hiding ((<>))

import Data.Text qualified as T
import Data.Time
import Text.PrettyPrint

data Hason
	= T T.Text | I Integer | Dbl Double | B Bool
	| UT UTCTime | ZT ZonedTime | DT NominalDiffTime
	| L [Hason] | Dct [(T.Text, Hason)]
	deriving (Show, Read)

ppr :: Hason -> Doc
ppr (T txt) = text "T" <+> text (show txt)
ppr (I n) = text "I" <+> integer n
ppr (Dbl x) = text "Dbl" <+> double x
ppr (B b) = text "B" <+> text (show b)
ppr (UT ut) = text "UT" <+> text (show ut)
ppr (ZT zt) = text "ZT" <+> text (show zt)
ppr (DT dt) = text "DT" <+> text (show dt)
ppr (L lst) = text "L" <+>
	hang (text "[") 2 (sep . punctuate (char ',') $ ppr <$> lst) <+>
	text "]"
ppr (Dct dct) = text "Dct" <+>
	hang (text "[") 2 (sep . punctuate (char ',') $ pprDict1 <$> dct) <+>
	text "]"

pprDict1 :: (T.Text, Hason) -> Doc
pprDict1 (k, v) = "(" <> text (show k) <> char ',' <+> ppr v <> ")"
