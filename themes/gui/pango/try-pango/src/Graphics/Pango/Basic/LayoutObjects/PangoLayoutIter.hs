{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.LayoutObjects.PangoLayoutIter (
	-- * TYPE
	PangoLayoutIter,

	-- * GET ITERATOR
	pangoLayoutGetIter,

	-- * NEXT
	pangoLayoutIterNextRun, pangoLayoutIterNextChar,
	pangoLayoutIterNextCluster, pangoLayoutIterNextLine,
	pangoLayoutIterAtLastLine,

	-- * GET FROM ITERATOR
	-- ** Get Index and Baseline
	pangoLayoutIterGetIndex, pangoLayoutIterGetBaseline,

	-- ** Get Run and Line
	pangoLayoutIterGetRun, pangoLayoutIterGetLine,

	-- ** Get Extents
	pangoLayoutIterGetCharExtents, pangoLayoutIterGetClusterExtents,
	pangoLayoutIterGetRunExtents,
	pangoLayoutIterGetLineYrange, pangoLayoutIterGetLineExtents ) where

import Graphics.Pango.Basic.LayoutObjects.PangoLayoutIter.Internal
