{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.NoThreadId where

import Control.Moffy.Internal.Sig
import Control.Moffy.Internal.React
import Control.Moffy.Internal.React.Type
import Data.Type.Set
import Data.Or

infixr 8 `first'`

first' :: Firstable es es' a b =>
	React s es a -> React s es' b -> React s (es :+: es') (Or a b)
first' = first_ noForkThreadId
