{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Freer.Par.Sequence (
	-- * Sequence and ViewL
	Sequence(..), ViewL(..),
	-- * Combinator
	(<|), (|>), mapS ) where

import Control.Arrow ((>>>))

---------------------------------------------------------------------------

-- * SEQUENCE AND VIEWL
-- * COMBINATOR

---------------------------------------------------------------------------
-- SEQUENCE AND VIEWL
---------------------------------------------------------------------------

infixr 8 ><

class Sequence sq where
	empty :: sq cat a a; singleton :: cat a b -> sq cat a b
	(><) :: sq cat a b -> sq cat b c -> sq cat a c
	viewl :: sq cat a b -> ViewL sq cat a b

data ViewL sq cat a b where
	EmptyL :: ViewL sq cat a a
	(:<|) :: cat a x -> sq cat x b -> ViewL sq cat a b

---------------------------------------------------------------------------
-- COMBINATOR
---------------------------------------------------------------------------

infixr 8 <|

(<|) :: Sequence sq => cat a b -> sq cat b c -> sq cat a c
c <| s = singleton c >< s

infixl 8 |>

(|>) :: Sequence sq => sq cat a b -> cat b c -> sq cat a c
s |> c = s >< singleton c

mapS :: (Applicative f, Sequence sq) =>
	(forall x y . cat x y -> f (cat x y)) -> sq cat a b -> f (sq cat a b)
mapS f = viewl >>> \case
	EmptyL -> pure empty; c :<| s -> (<|) <$> f c <*> f `mapS` s
