import DrawArea
import Prelude hiding (Left, Right)

main :: IO ()
main = do
	svg <- runArea 707 1000 $ do
		area0 <- mkArea 50 80 600 300
		(nameRubi, area1) <- hSepArea area0 True 20
		(name, area2) <- hSepArea area1 False 80
		(birth, area3) <- hSepArea area2 False 40
		addStr area0 (Left, Upper) False 20 "履 歴 書"
		addStr area0 (Right, Upper) False 15 "2013年 9月11日 現在"
		addStr nameRubi (Left, Middle) False 12 "ふりがな"
		addStr name (Left, Top) False 12 "氏 名"
		addStr birth (Left, Middle) False 12 "生年月日"
	putStr svg
