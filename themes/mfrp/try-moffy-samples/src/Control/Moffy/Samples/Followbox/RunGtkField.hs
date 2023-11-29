{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Followbox.RunGtkField (runFollowboxGen) where

import Control.Moffy
import Control.Moffy.Samples.Followbox.Event.CalcTextExtents
import Control.Moffy.Handle.TChan
import Control.Moffy.Run.TChan
import Control.Concurrent.STM
import Data.Type.Set ((:-), (:+:))
import System.Random

import Control.Moffy.Samples.Followbox.Event
import Control.Moffy.Samples.Followbox.Handle
import Trial.Followbox.TypeSynonym

runFollowboxGen ::
	TChan (EvReqs (CalcTextExtents :- GuiEv)) -> TChan (EvOccs (CalcTextExtents :- GuiEv)) -> String ->
	Maybe GithubNameToken -> TChan x -> Sig s FollowboxEv x r -> IO r
runFollowboxGen cr c brs mgnt c' s = do
	(r, _) <- interpretSt (handleFollowbox (cr, c) brs mgnt) c' s (initialFollowboxState $ mkStdGen 8)
	pure r

handleFollowbox ::
	(TChan (EvReqs (CalcTextExtents :- GuiEv)), TChan (EvOccs (CalcTextExtents :- GuiEv))) -> Browser ->
	Maybe GithubNameToken -> HandleF IO (CalcTextExtents :- GuiEv :+: FollowboxEv)
handleFollowbox = handleFollowboxWith (uncurry . handle)
