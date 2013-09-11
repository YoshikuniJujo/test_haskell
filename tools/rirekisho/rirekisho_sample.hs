import DrawArea
import Prelude hiding (Left, Right)

main :: IO ()
main = do
	svg <- runArea 707 1000 $ do
		baseArea0 <- mkArea 50 80 600 290
		(nameRubi, area1) <- hSepArea baseArea0 True 16
		(name, area2) <- hSepArea area1 False 64
		(area2_5, area3) <- hSepArea area2 False 32
		(birth, sex) <- vSepArea area2_5 False 420
		(area4, area5) <- hSepArea area3 False 56
		(area5_3, area5_5) <- vSepArea area4 False 460
		(addrRubi, addr) <- hSepArea area5_3 True 16
		(tel, fax) <- hSepArea area5_5 False 28
		(area6, area7) <- hSepArea area5 False 28
		(phoneT, area7_1) <- vSepArea area6 False 80
		(phone, area7_2) <- vSepArea area7_1 False 160
		(maddrT, maddr) <- vSepArea area7_2 False 80
		(area8, area9) <- hSepArea area7 False 40
		addStr baseArea0 (Left, Upper) False 20 "履 歴 書"
		addStr baseArea0 (Right, Upper) False 15 "2013年 9月11日 現在"
		addStr nameRubi (Left, Middle) False 10 "ふりがな"
		addStr name (Left, Top) False 10 "氏 名"
		(_, nameR2) <- vSepAreaLog nameRubi 180
		(nameR3, nameR4) <- vSepAreaLog nameR2 100
		(_, name2) <- vSepAreaLog name 150
		(name3, name4) <- vSepAreaLog name2 100
		addStr nameR3 (Left, Middle) False 10 "にっけい"
		addStr name3 (Left, Middle) True 35 "日経"
		addStr nameR4 (Left, Middle) False 10 "たろう"
		addStr name4 (Left, Middle) True 35 "太郎"
		addStr birth (Left, Middle) False 10 "生年月日"
		(_, birth1) <- vSepAreaLog birth 100
		(year, birth2) <- vSepAreaLog birth1 50
		(yearU, birth3) <- vSepAreaLog birth2 15
		(month, birth4) <- vSepAreaLog birth3 30
		(monthU, birth5) <- vSepAreaLog birth4 15
		(day, birth6) <- vSepAreaLog birth5 30
		(dayU, birth7) <- vSepAreaLog birth6 30
		(age, _) <- vSepAreaLog birth7 100
		(_, tel1) <- vSepAreaLog tel 30
		(_, fax1) <- vSepAreaLog fax 30
		addStr year (Center, Middle) False 17 "1974"
		addStr yearU (Center, Middle) False 17 "年"
		addStr month (Center, Middle) False 17 "8"
		addStr monthU (Center, Middle) False 17 "月"
		addStr day (Center, Middle) False 17 "16"
		addStr dayU (Center, Middle) False 17 "日生"
		addStr age (Left, Middle) False 17 "(満"
		addStr age (Center, Middle) False 17 "36"
		addStr age (Right, Middle) False 17 "歳)"
		addStr sex (Left, Middle) False 10 "性別"
		addStr sex (Center, Middle) False 17 "男"
		addStr addrRubi (Left, Middle) False 10 "ふりがな"
		addStr addr (Left, Top) False 10 "現 住 所"
		addStr tel (Left, Top) False 10 "TEL"
		addStr tel1 (Center, Middle) False 17 "03-XXXX-7330"
		addStr fax (Left, Top) False 10 "FAX"
		addStr fax1 (Center, Middle) False 17 "03-XXXX-6992"
		addStr phoneT (Center, Middle) False 15 "携帯電話"
		addStr phone (Center, Middle) False 17 "090-XXXX-1111"
		addStr maddrT (Center, Middle) False 15 "Email"
		addStr maddr (Center, Middle) False 17 "XXXX@nikkeihr.co.jp"

		baseArea1 <- mkArea 50 380 600 570
		return ()
	putStr svg

-- 都合の良い連絡方法と時間帯
