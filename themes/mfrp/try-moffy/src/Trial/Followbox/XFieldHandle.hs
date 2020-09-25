{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.XFieldHandle (
	-- * Handle
	HandleF, handleFollowbox,
	-- * State
	FollowboxState, initialFollowboxState ) where

import Control.Moffy.Event.Window
import Control.Moffy.Handle.XField (GuiEv, handle')
import Data.Type.Set ((:+:), (:-))

import Trial.Followbox.Event (FollowboxEv)
import Trial.Followbox.TypeSynonym (Browser, GithubNameToken)
import Trial.Followbox.Handle (
	HandleF, handleFollowboxWith, FollowboxState, initialFollowboxState )
import Field (Field)

---------------------------------------------------------------------------

handleFollowbox :: Field -> Browser -> Maybe GithubNameToken ->
	HandleF IO (WindowNew :- GuiEv :+: FollowboxEv)
handleFollowbox = handleFollowboxWith handle'
