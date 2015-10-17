data Rectangle
	= RectBR { topL :: (Double, Double), botR :: (Double, Double) }
	| RectWH { topL :: (Double, Double), width :: Double, height :: Double }
	deriving Show
