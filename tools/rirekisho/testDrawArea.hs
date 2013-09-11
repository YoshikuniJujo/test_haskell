import DrawArea

main :: IO ()
main = do
	svg <- runArea 707 1000 $ do
		area <- mkArea 50 50 600 900
		addStr area (Center, Middle) True 50 "今日は"
	putStr svg
