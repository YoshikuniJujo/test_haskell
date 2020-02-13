{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Sequence where

class Sequence sq where
	empty :: sq cat a a
	singleton :: cat a b -> sq cat a b
	(><) :: sq cat a b -> sq cat b c -> sq cat a c
	viewl :: sq cat a b -> Viewl sq cat a b

data Viewl sq cat a b where
	EmptyL :: Viewl sq cat a a
	(:<|) :: cat a x -> sq cat x b -> Viewl sq cat a b

(|>) :: Sequence sq => sq cat a b -> cat b c -> sq cat a c
s |> c = s >< singleton c
