{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.TypeLevel where

data V2 t ss where V2 :: { unV2 :: t s1 s2 } -> V2 t '(s1, s2)
data V3 t ss where V3 :: { unV3 :: t s1 s2 s3 } -> V3 t '(s1, s2, s3)
data V4 t ss where V4 :: t s1 s2 s3 s4 -> V4 t '(s1, s2, s3, s4)
data V5 t ss where V5 :: t s1 s2 s3 s4 s5 -> V5 t '(s1, s2, s3, s4, s5)
data V6 t ss where V6 :: t s1 s2 s3 s4 s5 s6 -> V6 t '(s1, s2, s3, s4, s5, s6)

data V12 t ss where
	V12 :: t s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 -> V12 t
		'(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12)

data V13 t ss where
	V13 :: t s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 -> V13 t
		'(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13)

data V14 t ss where
	V14 :: t s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 -> V14 t
		'(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14)

data V15 t ss where
	V15 :: t s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 -> V15 t '(
		s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15
		)

deriving instance Show (t s1 s2) => Show (V2 t '(s1, s2))
deriving instance Show (t s1 s2 s3) => Show (V3 t '(s1, s2, s3))
deriving instance Show (t s1 s2 s3 s4) => Show (V4 t '(s1, s2, s3, s4))
deriving instance Show (t s1 s2 s3 s4 s5) => Show (V5 t '(s1, s2, s3, s4, s5))
deriving instance Show (t s1 s2 s3 s4 s5 s6) => Show (V6 t '(s1, s2, s3, s4, s5, s6))

deriving instance Show (t s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12) =>
	Show (V12 t
		'(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12))

deriving instance Show (t s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13) =>
	Show (V13 t
		'(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13))

deriving instance Show (t s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14) =>
	Show (V14 t
		'(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14))

deriving instance Show (t s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15) =>
	Show (V15 t '(
		s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15
		))
