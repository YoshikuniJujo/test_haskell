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
		(paddrT, area10) <- vSepArea area8 False 80
		(paddr, area11) <- vSepArea area10 False 160
		(contactT, contact) <- vSepArea area11 False 80
		(area12, area13) <- vSepArea area9 False 460
		(contRubi, cont) <- hSepArea area12 True 16
		(cTel, cFax) <- hSepArea area13 False 28
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
		(_, addrR1) <- vSepAreaLog addrRubi 50
		(_, addr1) <- vSepAreaLog addr 50
		(post, addr2) <- vSepAreaLog addr1 100
		(post1, post1_) <- vSepAreaLog post 20
		(post2, post3) <- vSepAreaLog post1_ 60
		(post2_1, post2_1_) <- vSepAreaLog post2 30
		(post2_2, post2_3) <- vSepAreaLog post2_1_ 15
		(_, tel1) <- vSepAreaLog tel 30
		(_, fax1) <- vSepAreaLog fax 30
		(_, cont1) <- vSepAreaLog cont 50
		(cpost, cont2) <- vSepAreaLog cont1 100
		(cpost1, cpost1_) <- vSepAreaLog cpost 20
		(cpost2, cpost3) <- vSepAreaLog cpost1_ 60
		(cpost2_1, cpost2_1_) <- vSepAreaLog cpost2 30
		(cpost2_2, cpost2_3) <- vSepAreaLog cpost2_1_ 15
		(paddrT1, paddrT2) <- hSepAreaLog paddrT 20
		(contactT1, contactT2) <- hSepAreaLog contactT 20
		(contact1, contact2) <- hSepAreaLog contact 20
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
		addStr addrR1 (Center, Middle) False 10 $
			"とうきょうとちよだくかんだかじちょう"
		addStr addr (Left, Top) False 10 "現 住 所"
		addStr post1 (Left, Top) False 12 "〒("
--		addStr post2 (Center, Top) False 12 "000-0145"
		addStr post2_1 (Center, Top) False 12 "000"
		addStr post2_2 (Center, Top) False 12 "-"
		addStr post2_3 (Center, Top) False 12 "0145"
		addStr post3 (Right, Top) False 12 ")"
		addStr addr1 (Center, Middle) False 14
			"東京都千代田区神田鍛治町3-6-X ポヨンD棟102号室"
		addStr tel (Left, Top) False 10 "TEL"
		addStr tel1 (Center, Middle) False 17 "03-XXXX-7330"
		addStr fax (Left, Top) False 10 "FAX"
		addStr fax1 (Center, Middle) False 17 "03-XXXX-6992"
		addStr phoneT (Center, Middle) False 15 "携帯電話"
		addStr phone (Center, Middle) False 17 "090-XXXX-1111"
		addStr maddrT (Center, Middle) False 15 "Email"
		addStr maddr (Center, Middle) False 17 "XXXX@nikkeihr.co.jp"
		addStr paddrT1 (Center, Middle) False 15 "携帯電話"
		addStr paddrT2 (Center, Middle) False 10 "メールアドレス"
		addStr paddr (Center, Middle) False 17 "nikkei@XX.ne.jp"
		addStr contactT1 (Left, Middle) False 8.5 "都合の良い"
		addStr contactT2 (Left, Top) False 8.5 "連絡方法と時間帯"
		addStr contact1 (Left, Middle) False 10
			"携帯電話のメールにご連絡お願いします。"
		addStr contact2 (Left, Top) False 10
			"電話連絡も携帯で、17時以降が都合がよいです。"
		addStr contRubi (Left, Middle) False 10 "ふりがな"
		addStr cTel (Left, Top) False 10 "TEL"
		addStr cFax (Left, Top) False 10 "FAX"
		addStr cont (Left, Top) False 10 "連 絡 先"
		addStr cpost1 (Left, Top) False 12 "〒("
		addStr cpost2_2 (Center, Top) False 12 "-"
		addStr cpost3 (Right, Top) False 12 ")"
		addStr cont2 (Center, Top) False 12
			"(現住所以外に連絡を希望する場合のみ記入)"

		baseArea1 <- mkArea 50 380 600 570
		return ()
	putStr svg

-- 都合の良い連絡方法と時間帯
