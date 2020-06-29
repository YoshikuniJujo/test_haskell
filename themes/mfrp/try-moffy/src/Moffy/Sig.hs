{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.Sig where

import Moffy.React

infixr 5 :|
newtype Sig s es a r = Sig { unSig :: React s es (ISig s es a r) }
data ISig s es a r = End r | a :| Sig s es a r

isig :: (r -> b) -> (a -> Sig s es a r -> b) -> ISig s es a r -> b
isig e c = \case End x -> e x; h :| t -> c h t

instance Functor (Sig s es a) where f `fmap` Sig s = Sig $ (f <$>) <$> s

instance Functor (ISig s es a) where
	fmap f = isig (End . f) (\h -> (h :|) . (f <$>))

{-
hold :: Sig s es a r
hold = waitFor never

waitFor :: React s es r -> Sig s es a r
waitFor = Sig . (pure <$>)
-}
