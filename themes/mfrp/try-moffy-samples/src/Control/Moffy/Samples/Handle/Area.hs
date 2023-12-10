{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Handle.Area where

import Control.Concurrent.STM
import Control.Moffy.Handle
import Control.Moffy.Samples.Event.Area
import Data.Type.Set
import Data.OneOrMore qualified as Oom
import Data.OneOrMoreApp qualified as App
import Data.Map qualified as M

handle :: TVar (M.Map Int (Point, Point)) -> Handle' IO (SetArea :- Singleton GetArea)
handle vm = handleSetArea vm `merge` handleGetArea vm

handleSetArea :: TVar (M.Map Int (Point, Point)) -> Handle' IO (Singleton SetArea)
handleSetArea vm (Oom.Singleton (SetAreaReq i ul dr)) = do
	atomically . modifyTVar vm $ M.insert i (ul, dr)
	pure . Just $ App.Singleton OccSetArea

handleGetArea :: TVar (M.Map Int (Point, Point)) -> Handle' IO (Singleton GetArea)
handleGetArea vm (Oom.Singleton (GetAreaReq i)) = do
	Just . App.Singleton . uncurry (OccGetArea i) <$> atomically ((M.! i) <$> readTVar vm)
