{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.XFieldHandle (
	-- * Handle
	HandleF, handleFollowbox,
	-- * State
	FollowboxState, initialFollowboxState ) where

import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Handle.XField (GuiEv, handle')
import Data.Type.Set ((:+:), (:-))

import Trial.Followbox.Event (FollowboxEv)
import Trial.Followbox.TypeSynonym (Browser, GithubNameToken)
import Trial.Followbox.Handle (
	HandleF, handleFollowboxWith, FollowboxState, initialFollowboxState )
import Field (Field)

import Control.Moffy.Event.Cursor

---------------------------------------------------------------------------

handleFollowbox :: Field -> Browser -> Maybe GithubNameToken ->
	HandleF IO (CursorEv :+: WindowNew :- DefaultWindowEv :+: GuiEv :+: FollowboxEv)
handleFollowbox = handleFollowboxWith handle'
