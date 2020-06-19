{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Boxes.Handle (Mode(InitMode), handleBoxes) where

import Control.Monad.State (StateT, get, put, lift)
import Data.UnionSet (prj, singleton, (>-), expand)
import Data.Time (DiffTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (AbsoluteTime, diffAbsoluteTime, addAbsoluteTime)
	
import MonadicFrp.Handle (
	HandleSt, HandleSt', retrySt, expandSt, mergeSt )
import MonadicFrp.XFieldHandle.Mouse (handleMouse)
import Trials.Boxes.Event (GuiEv, MouseEv, TimeEv, TryWait(..), Occurred(..))
import Field (Field)

---------------------------------------------------------------------------

data Mode = InitMode | WaitMode AbsoluteTime deriving Show

handleBoxes :: DiffTime -> Field -> HandleSt Mode (StateT AbsoluteTime IO) GuiEv
handleBoxes prd f = retrySt \case
	InitMode -> handleInit (prd, f); WaitMode now -> handleWait now

handleInit :: HandleSt' (DiffTime, Field) Mode (StateT AbsoluteTime IO) GuiEv
handleInit = mergeSt
	handleMouse' (\_ -> pure ())
	handleNow (\_ -> InitMode <$ (put =<< lift getTaiTime))

handleMouse' :: HandleSt' (DiffTime, Field) () (StateT AbsoluteTime IO) MouseEv
handleMouse' (prd, f) reqs = lift $ (, ()) <$> handleMouse (Just prd) f reqs

handleNow :: HandleSt' () Mode (StateT AbsoluteTime IO) TimeEv
handleNow () reqs = (`handleTime` reqs) =<< lift getTaiTime

getTaiTime :: IO AbsoluteTime
getTaiTime = systemToTAITime <$> getSystemTime

handleWait :: Monad m =>
	HandleSt' AbsoluteTime Mode (StateT AbsoluteTime m) GuiEv
handleWait = expandSt handleTime ((InitMode <$) . put)

handleTime :: Monad m =>
	HandleSt' AbsoluteTime Mode (StateT AbsoluteTime m) TimeEv
handleTime now reqs = get >>= \lst -> do
	let	dt = now `diffAbsoluteTime` lst
		odt = singleton $ OccDeltaTime dt
	case prj reqs of
		Just (TryWaitReq t)
			| t < dt -> (Just $ OccTryWait t >- odt', WaitMode now)
				<$ put (t `addAbsoluteTime` lst)
			| otherwise -> (Just $ OccTryWait dt >- odt, InitMode)
				<$ put now
			where odt' = singleton $ OccDeltaTime t
		Nothing -> (Just . expand $ odt, InitMode) <$ put now
