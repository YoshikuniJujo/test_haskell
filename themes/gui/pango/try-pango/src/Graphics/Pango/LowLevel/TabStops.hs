{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.LowLevel.TabStops (
	-- * PANGO TAB ARRAY
	PangoTabArray, pangoTabArrayThaw,
	pangoTabArrayGetTab, pangoTabArrayGetTabs,

	-- * PANGO TAB ARRAY NULLABLE
	PangoTabArrayNullable,
	pangoTabArrayFromNullable, pangoTabArrayToNullable,

	-- * PANGO TAB ARRAY FIXED
	PangoTabArrayFixed, pangoTabArrayFixedFreeze,
	pangoTabArrayFixedNew, pangoTabArrayFixedSetTab,

	-- * PANGO TAB ARRAY INT
	PangoTabArrayInt, pangoTabArrayIntFreeze,
	pangoTabArrayIntNew, pangoTabArrayIntSetTab,
	) where

import Graphics.Pango.LowLevel.TabStops.Internal
