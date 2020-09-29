{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Cursor where

import Control.Moffy
import Control.Moffy.Event.Window
import Data.Type.Set
import Data.Bool

import qualified Data.ByteString as BS

data Result = Failure | Success deriving (Show, Eq, Ord)

data NamedCursor
	= None | Default | Help | Pointer | ContextMenu | Progress | Wait
	| Cell | Crosshair | Text | VerticalText | Alias | Copy | NoDrop
	| Move | NotAllowed | Grab | Grabbing | AllScroll | ColResize
	| RowResize | NResize | EResize | SResize | WResize | NeResize
	| NwResize | SwResize | SeResize | EwResize | NsResize | NeswResize
	| NwseResize | ZoomIn | ZoomOut deriving (Show, Eq, Ord)

data SetCursorFromName = SetCursorFromNameReq WindowId NamedCursor deriving (Show, Eq, Ord)
numbered [t| SetCursorFromName |]
instance Request SetCursorFromName where
	data Occurred SetCursorFromName = OccSetCursorFromName WindowId NamedCursor Result deriving Show

setCursorFromName :: WindowId -> NamedCursor -> React s (Singleton SetCursorFromName) Result
setCursorFromName wid0 nc0 = maybe (setCursorFromName wid0 nc0) pure =<<
	await (SetCursorFromNameReq wid0 nc0)
		\(OccSetCursorFromName wid nc r) -> bool Nothing (Just r) $ wid == wid0 && nc == nc0

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

data PngCursor = PngCursor Double Double BS.ByteString deriving (Show, Eq, Ord)

data SetCursorFromPng = SetCursorFromPngReq WindowId PngCursor deriving (Show, Eq, Ord)
numbered [t| SetCursorFromPng |]
instance Request SetCursorFromPng where
	data Occurred SetCursorFromPng = OccSetCursorFromPng WindowId PngCursor Result deriving Show

setCursorFromPng :: WindowId -> PngCursor -> React s (Singleton SetCursorFromPng) Result
setCursorFromPng wid0 pc0 = maybe (setCursorFromPng wid0 pc0) pure =<<
	await (SetCursorFromPngReq wid0 pc0)
		\(OccSetCursorFromPng wid pc r) -> bool Nothing (Just r) $ wid == wid0 && pc == pc0

type CursorEv = SetCursorFromName :- SetCursorFromPng :- 'Nil
