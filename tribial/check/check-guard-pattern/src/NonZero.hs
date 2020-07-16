{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module NonZero where

pattern Zero :: Integer
pattern Zero = 0

{-# COMPLETE Zero, NonZero #-}

pattern NonZero :: Integer -> Integer
pattern NonZero n <- (nonZero -> Just n)
	where NonZero = id

nonZero :: Integer -> Maybe Integer
nonZero = \case 0 -> Nothing; n -> Just n

foo :: Integer -> Integer
foo (NonZero n) = 123 `div` n
foo 0 = 321
