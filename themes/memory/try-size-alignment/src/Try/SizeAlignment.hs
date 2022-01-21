{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.SizeAlignment where

import GHC.Generics
import Foreign.Storable.SizeAlignment

data Foo = Foo Bool Int Double deriving (Show, Generic)
instance SizeAlignmentList Foo
instance MapStorableUntil t Foo => SizeAlignmentListUntil t Foo
