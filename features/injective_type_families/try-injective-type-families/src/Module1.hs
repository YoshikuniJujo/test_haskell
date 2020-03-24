{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Module1 where

import Injective

type instance Foo Bool = Integer
