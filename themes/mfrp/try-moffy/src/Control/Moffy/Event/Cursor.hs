{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Cursor where

import Control.Moffy
import Data.Type.Set
import Data.Bool

data Result = Failure | Success deriving (Show, Eq, Ord)

data NamedCursor
	= None | Default | Help | Pointer | ContextMenu | Progress | Wait
	| Cell | Crosshair | Text | VerticalText | Alias | Copy | NoDrop
	| Move | NotAllowed | Grab | Grabbing | AllScroll | ColResize
	| RowResize | NResize | EResize | SResize | WResize | NeResize
	| NwResize | SwResize | SeResize | EwResize | NsResize | NeswResize
	| NwseResize | ZoomIn | ZoomOut deriving (Show, Eq, Ord)

newtype SetCursorFromName = SetCursorFromNameReq NamedCursor deriving (Show, Eq, Ord)
numbered [t| SetCursorFromName |]
instance Request SetCursorFromName where
	data Occurred SetCursorFromName = OccSetCursorFromName NamedCursor Result deriving Show

setCursorFromName :: NamedCursor -> React s (Singleton SetCursorFromName) Result
setCursorFromName nc0 = maybe (setCursorFromName nc0) pure =<<
	await (SetCursorFromNameReq nc0)
		\(OccSetCursorFromName nc r) -> bool Nothing (Just r) $ nc == nc0

cursorName :: NamedCursor -> String
cursorName = \case
	None -> "none"; Default -> "default"; Help -> "help"
	Pointer -> "pointer"; ContextMenu -> "context-menu"
	Progress -> "progress"; Wait -> "wait"; Cell -> "cell"
	Crosshair -> "crosshair"; Text -> "text"
	VerticalText -> "vertical-text"; Alias -> "alias"; Copy -> "copy"
	NoDrop -> "no-drop"; Move -> "move"; NotAllowed -> "not-allowed"
	Grab -> "grab"; Grabbing -> "grabbing"; AllScroll -> "all-scroll"
	ColResize -> "col-resize"; RowResize -> "row-resize"
	NResize -> "n-resize"; EResize -> "e-resize"; SResize -> "s-resize"
	WResize -> "w-resize"; NeResize -> "we-resize"; NwResize -> "nw-resize"
	SwResize -> "sw-resize"; SeResize -> "se-resize"
	EwResize -> "ew-resize"; NsResize -> "ns-resize"
	NeswResize -> "nesw-resize"; NwseResize -> "nwse-resize"
	ZoomIn -> "zoom-in"; ZoomOut -> "zoom-out"
