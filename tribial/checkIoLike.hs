{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad.State
import Data.Time.Clock.TAI
import Data.Time.Clock.System

class TaiMonad m where
	getAbsoluteTime :: m AbsoluteTime

instance TaiMonad IO where
	getAbsoluteTime = systemToTAITime <$> getSystemTime

newtype AbsoluteTimeList a = AbsoluteTimeList (State [AbsoluteTime] a)
	deriving (Functor, Applicative, Monad, MonadState [AbsoluteTime])

instance TaiMonad AbsoluteTimeList where
	getAbsoluteTime = gets head <* modify tail
