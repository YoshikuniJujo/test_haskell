{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.XFieldHandle (
	-- * Handle
	HandleF, handleFollowbox,
	-- * State
	FollowboxState, initialFollowboxState ) where

import Control.Moffy.Handle (liftHandle', retrySt, beforeSt, mergeSt)
import Control.Moffy.Handle.ThreadId (handleGetThreadId)
import Control.Moffy.Handle.Lock (handleLock)
import Control.Moffy.Handle.Random (handleRandom)
import Control.Moffy.Handle.XField (GuiEv, handle')
import Data.Type.Set ((:+:))

import Trial.Followbox.Event (FollowboxEv)
import Trial.Followbox.TypeSynonym (Browser, GithubNameToken)
import Field (Field)

import Trial.Followbox.Handle

---------------------------------------------------------------------------

-- * STATE
-- 	+ FOLLOWBOX STATE
-- 	+ PUT AND GET EACH STATE
-- * HANDLE
--	+ FOLLOWBOX
--	+ MOUSE
--	+ STORE AND LOAD JSONS
--	+ REQUEST DATA
--	+ BROWSE
--	+ BEGIN AND END SLEEP
--	+ RAISE ERROR
-- * HELPER FUNCTION

---------------------------------------------------------------------------
-- HANDLE
---------------------------------------------------------------------------

-- FOLLOWBOX

handleFollowbox :: Field -> Browser -> Maybe GithubNameToken ->
	HandleF IO (GuiEv :+: FollowboxEv)
handleFollowbox f brws mba = retrySt $
	liftHandle' handleGetThreadId `mergeSt` handleLock `mergeSt`
	handleRandom `mergeSt`
	handleStoreJsons `mergeSt` handleLoadJsons `mergeSt`
	liftOnJust (handleHttpGet mba) `mergeSt`
	liftOnJust handleGetTimeZone `mergeSt`
	liftOnJust (handleBrowse brws) `mergeSt`
	handleBeginSleep `mergeSt` handleEndSleep `mergeSt`
	liftHandle' handleRaiseError `beforeSt` handleMouseWithSleep handle' f
