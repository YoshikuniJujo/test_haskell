{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.Handle (Mode(InitMode), handleBoxes) where

import Control.Concurrent
import Control.Moffy.Handle hiding (expand)
import Control.Monad.State (StateT, get, put, lift, liftIO)
import Data.Type.Set
import Data.OneOrMore (project, pattern Singleton, (>-), expand)
import Data.Time (DiffTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (AbsoluteTime, diffAbsoluteTime, addAbsoluteTime)
	
import Control.Moffy.Handle.XField.Mouse
import Control.Moffy.Handle.XField
import Control.Moffy.Event.Delete
import Trial.Boxes.Event (GuiEv, MouseEv, TimeEv, TryWait(..), Occurred(..))
import Field (Field)

---------------------------------------------------------------------------

handleMouse :: Maybe DiffTime -> Field -> Handle' IO (DeleteEvent :- MouseEv)
handleMouse = handleXField \case MouseEv e -> Just $ expand e; _ -> Nothing

data Mode = InitMode | WaitMode AbsoluteTime deriving Show

handleBoxes :: DiffTime -> Field -> HandleSt Mode (StateT AbsoluteTime IO) GuiEv
handleBoxes prd f rqs = (`retrySt` rqs) \rqs' -> \case
	InitMode -> (rqs' `handleInit` (prd, f)); WaitMode now -> (rqs' `handleWait` now)

handleInit :: HandleSt' (DiffTime, Field) Mode (StateT AbsoluteTime IO) GuiEv
handleInit = mergeSt
	handleMouse' (\(prd, _) -> liftIO . threadDelay . round $ prd * 1000000)
	handleNow (\_ -> InitMode <$ (put =<< lift getTaiTime))

handleMouse' :: HandleSt' (DiffTime, Field) () (StateT AbsoluteTime IO) (DeleteEvent :- MouseEv)
handleMouse' reqs (prd, f) = lift $ (, ()) <$> handleMouse (Just prd) f reqs

handleNow :: HandleSt' () Mode (StateT AbsoluteTime IO) TimeEv
handleNow reqs () = (reqs `handleTime`) =<< lift getTaiTime

getTaiTime :: IO AbsoluteTime
getTaiTime = systemToTAITime <$> getSystemTime

handleWait :: Monad m =>
	HandleSt' AbsoluteTime Mode (StateT AbsoluteTime m) GuiEv
handleWait = expandSt handleTime ((InitMode <$) . put)

handleTime :: Monad m =>
	HandleSt' AbsoluteTime Mode (StateT AbsoluteTime m) TimeEv
handleTime reqs now = get >>= \lst -> do
	let	dt = now `diffAbsoluteTime` lst
		odt = Singleton $ OccDeltaTime dt
	case project reqs of
		Just (TryWaitReq t)
			| t < dt -> (Just $ OccTryWait t >- odt', WaitMode now)
				<$ put (t `addAbsoluteTime` lst)
			| otherwise -> (Just $ OccTryWait dt >- odt, InitMode)
				<$ put now
			where odt' = Singleton $ OccDeltaTime t
		Nothing -> (Just . expand $ odt, InitMode) <$ put now
