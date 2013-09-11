import DrawArea
import Prelude hiding (Left, Right)

main :: IO ()
main = do
	svg <- runArea 707 1000 $ do
		area <- mkArea 50 50 600 900
		(area1, area2) <- hSepArea area False 25
		(area3, area4) <- vSepArea area2 True 25
		addStr area1 (Center, Middle) True 50 "今日は"
		addStr area3 (Center, Middle) False 25 "さようなら"
		addStr area4 (Left, Upper) True 25 "さやうなら"
		addStr area4 (Left, Top) False 25 "さやうなら"
		addStr area1 (Left, Lower) False 25 "さやうなら"
		addStr area1 (Left, Bottom) True 25 "さやうなら"
		addStr area3 (Right, Bottom) True 25 "さやうなら"
	putStr svg
