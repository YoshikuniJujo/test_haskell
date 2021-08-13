{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Cursors (
	-- * GDK CURSOR
	GdkCursor,

	-- * DISPLAY
	gdkCursorGetDisplay,

	-- * CURSOR NEW
	-- ** From Surface
	gdkCursorNewFromSurface,
	-- ** From Name
	gdkCursorNewFromName,
	-- ** From Cursor Type
	gdkCursorNewForDisplay, gdkCursorGetCursorType,
	-- *** GDK CURSOR TYPE
	GdkCursorType,
	pattern GdkXCursor, pattern GdkArrow,
	pattern GdkBasedArrowDown, pattern GdkBasedArrowUp,
	pattern GdkBoat, pattern GdkBogosity,
	pattern GdkBottomLeftCorner, pattern GdkBottomRightCorner,
	pattern GdkBottomSide, pattern GdkBottomTee,
	pattern GdkBoxSpiral, pattern GdkCenterPtr,
	pattern GdkCircle, pattern GdkClock,
	pattern GdkCoffeeMug, pattern GdkCross,
	pattern GdkCrossReverse, pattern GdkCrosshair,
	pattern GdkDiamondCross, pattern GdkDot,
	pattern GdkDotbox, pattern GdkDoubleArrow,
	pattern GdkDraftLarge, pattern GdkDraftSmall,
	pattern GdkDrapedBox, pattern GdkExchange,
	pattern GdkFleur, pattern GdkGobbler,
	pattern GdkGumby, pattern GdkHand1,
	pattern GdkHand2, pattern GdkHeart,
	pattern GdkIcon, pattern GdkIronCross,
	pattern GdkLeftPtr, pattern GdkLeftSide,
	pattern GdkLeftTee, pattern GdkLeftbutton,
	pattern GdkLlAngle, pattern GdkLrAngle,
	pattern GdkMan, pattern GdkMiddlebutton,
	pattern GdkMouse, pattern GdkPencil,
	pattern GdkPirate, pattern GdkPlus,
	pattern GdkQuestionArrow, pattern GdkRightPtr,
	pattern GdkRightSide, pattern GdkRightTee,
	pattern GdkRightbutton, pattern GdkRtlLogo,
	pattern GdkSailboat, pattern GdkSbDownArrow,
	pattern GdkSbHDoubleArrow, pattern GdkSbLeftArrow,
	pattern GdkSbRightArrow, pattern GdkSbUpArrow,
	pattern GdkSbVDoubleArrow, pattern GdkShuttle,
	pattern GdkSizing, pattern GdkSpider,
	pattern GdkSpraycan, pattern GdkStar,
	pattern GdkTarget, pattern GdkTcross,
	pattern GdkTopLeftArrow, pattern GdkTopLeftCorner,
	pattern GdkTopRightCorner, pattern GdkTopSide,
	pattern GdkTopTee, pattern GdkTrek,
	pattern GdkUlAngle, pattern GdkUmbrella,
	pattern GdkUrAngle, pattern GdkWatch,
	pattern GdkXterm, pattern GdkBlankCursor,
	-- *** GDK NO CURSOR TYPE
	GdkNoCursorType,
	pattern GdkLastCursor, pattern GdkCursorIsPixmap

	) where

import Graphics.Gdk.Cursors.Internal
