import DrawArea
import Parse
import Tools
import Prelude hiding (Left, Right)
import qualified Prelude as P
-- import Control.Applicative
import Control.Monad
import Data.Maybe

main :: IO ()
main = do
	dat <- readData "rirekisho.dat"
	svg <- runArea 3535 5000 $ do
		mkBaseArea0 dat
		mkBaseArea1 dat
	putStr svg

mkBaseArea0 :: [(String, Either String [String])] -> AreaM ()
mkBaseArea0 dat = do
	baseArea0 <- mkArea 250 400 3000 1450
	(nameRubi, area1) <- hSepArea baseArea0 True 80
	(name, area2) <- hSepArea area1 False 320
	(area2_5, area3) <- hSepArea area2 False 160
	(birth, sex) <- vSepArea area2_5 False 2100
	(area4, area5) <- hSepArea area3 False 280
	(area5_3, area5_5) <- vSepArea area4 False 2300
	(addrRubi, addr) <- hSepArea area5_3 True 80
	(tel, fax) <- hSepArea area5_5 False 140
	(area6, area7) <- hSepArea area5 False 140
	(phoneT, area7_1) <- vSepArea area6 False 400
	(phone, area7_2) <- vSepArea area7_1 False 800
	(maddrT, maddr) <- vSepArea area7_2 False 450
	(area8, area9) <- hSepArea area7 False 200
	(paddrT, area10) <- vSepArea area8 False 400
	(paddr, area11) <- vSepArea area10 False 800
	(contactT, contact) <- vSepArea area11 False 450
	(area12, area13) <- vSepArea area9 False 2300
	(contRubi, cont) <- hSepArea area12 True 80
	(cTel, cFax) <- hSepArea area13 False 140
	addStr baseArea0 (Left, Upper) False 100 "履 歴 書"
	addStr baseArea0 (Right, Upper) False 75 "2013年 9月11日 現在"
	addStr nameRubi (Left, Middle) False 50 "ふりがな"
	addStr name (Left, Top) False 50 "氏 名"
	(_, nameR2) <- vSepAreaLog nameRubi 900
	(nameR3, nameR4) <- vSepAreaLog nameR2 500
	(_, name2) <- vSepAreaLog name 750
	(name3, name4) <- vSepAreaLog name2 500
	addStr nameR3 (Left, Middle) False 50 $ getVal "miyojiRubi"
	addStr name3 (Left, Middle) True 175 $ getVal "miyoji"
	addStr nameR4 (Left, Middle) False 50 $ getVal "namaeRubi"
	addStr name4 (Left, Middle) True 175 $ getVal "namae"
	addStr birth (Left, Middle) False 50 "生年月日"
	(_, birth1) <- vSepAreaLog birth 500
	(year, birth2) <- vSepAreaLog birth1 250
	(yearU, birth3) <- vSepAreaLog birth2 75
	(month, birth4) <- vSepAreaLog birth3 150
	(monthU, birth5) <- vSepAreaLog birth4 75
	(day, birth6) <- vSepAreaLog birth5 150
	(dayU, birth7) <- vSepAreaLog birth6 150
	(age, _) <- vSepAreaLog birth7 500
	(_, addrR1) <- vSepAreaLog addrRubi 250
	(_, addr1) <- vSepAreaLog addr 250
	(post, addr2) <- vSepAreaLog addr1 500
	(post1, post1_) <- vSepAreaLog post 100
	(post2, post3) <- vSepAreaLog post1_ 300
	(post2_1, post2_1_) <- vSepAreaLog post2 150
	(post2_2, post2_3) <- vSepAreaLog post2_1_ 75
	(_, tel1) <- vSepAreaLog tel 150
	(_, fax1) <- vSepAreaLog fax 150
	(_, cont1) <- vSepAreaLog cont 250
	(cpost, cont2) <- vSepAreaLog cont1 500
	(cpost1, cpost1_) <- vSepAreaLog cpost 100
	(cpost2, cpost3) <- vSepAreaLog cpost1_ 300
	(cpost2_1, cpost2_1_) <- vSepAreaLog cpost2 150
	(cpost2_2, cpost2_3) <- vSepAreaLog cpost2_1_ 75
	(paddrT1, paddrT2) <- hSepAreaLog paddrT 100
	(contactT1, contactT2) <- hSepAreaLog contactT 100
	(contact1, contact2) <- hSepAreaLog contact 100
	let [y, m, d, a] = getVals "birth"
	addStr year (Center, Middle) False 85 y
	addStr yearU (Center, Middle) False 85 "年"
	addStr month (Center, Middle) False 85 m
	addStr monthU (Center, Middle) False 85 "月"
	addStr day (Center, Middle) False 85 d
	addStr dayU (Center, Middle) False 85 "日生"
	addStr age (Left, Middle) False 85 "(満"
	addStr age (Center, Middle) False 85 a
	addStr age (Right, Middle) False 85 "歳)"
	addStr sex (Left, Middle) False 50 "性別"
	addStr sex (Center, Middle) False 85 $ getVal "sex"
	addStr addrRubi (Left, Middle) False 50 "ふりがな"
	addStr addrR1 (Center, Middle) False 50 $ getVal "addressRubi"
	addStr addr (Left, Top) False 50 "現 住 所"
	addStr post1 (Left, Top) False 60 "〒("
--	addStr post2 (Center, Top) False 12 "000-0145"
	addStr post2_1 (Center, Top) False 60 $ getVal "ynum1"
	addStr post2_2 (Center, Top) False 60 "-"
	addStr post2_3 (Center, Top) False 60 $ getVal "ynum2"
	addStr post3 (Right, Top) False 60 ")"
	addStr addr1 (Center, Middle) False 70 $ getVal "address"
	addStr tel (Left, Top) False 50 "TEL"
	addStr tel1 (Center, Middle) False 85 $ getVal "tel"
	addStr fax (Left, Top) False 50 "FAX"
	addStr fax1 (Center, Middle) False 85 $ getVal "fax"
	addStr phoneT (Center, Middle) False 75 "携帯電話"
	addStr phone (Center, Middle) False 85 $ getVal "pphone"
	addStr maddrT (Center, Middle) False 75 "Email"
	addStr maddr (Center, Middle) False 85 $ getVal "email"
	addStr paddrT1 (Center, Middle) False 75 "携帯電話"
	addStr paddrT2 (Center, Middle) False 50 "メールアドレス"
	addStr paddr (Center, Middle) False 60 $ getVal "pemail"
	addStr contactT1 (Left, Middle) False 50 "都合の良い"
	addStr contactT2 (Left, Top) False 50 "連絡方法と時間帯"
	addStr contact1 (Left, Middle) False 50 $ getVal "tocontactme1"
	addStr contact2 (Left, Top) False 50 $ getVal "tocontactme2"
	addStr contRubi (Left, Middle) False 50 "ふりがな"
	addStr cTel (Left, Top) False 50 "TEL"
	addStr cFax (Left, Top) False 50 "FAX"
	addStr cont (Left, Top) False 50 "連 絡 先"
	addStr cpost1 (Left, Top) False 60 "〒("
	addStr cpost2_2 (Center, Top) False 60 "-"
	addStr cpost3 (Right, Top) False 60 ")"
	addStr cont2 (Center, Top) False 60
		"(現住所以外に連絡を希望する場合のみ記入)"
	where
	getVal t = let P.Left v = fromJust $ lookup t dat in v
	getVals t = let P.Right vs = fromJust $ lookup t dat in vs

mkBaseArea1 :: [(String, Either String [String])] -> AreaM ()
mkBaseArea1 dat = do
	baseArea1 <- mkArea 250 1900 3000 2840
	(area0, area1) <- hSepArea baseArea1 False 120
	(year, area2) <- vSepArea area0 True 400
	(month, title) <- vSepArea area2 False 200
	addStr year (Center, Middle) False 60 "年"
	addStr month (Center, Middle) False 60 "月"
	addStr title (Center, Middle) False 60
		"学歴・職歴など(項目順にまとめて書く)"
	has <- unfoldrM mkHistArea area1
	zipWithM addHist has $ [
		Title "学歴"] ++
		readHists (getVals "school") ++ [
		Title "職歴"] ++
		readHists (getVals "jobs") ++ [
		Title "賞罰"] ++
		readHists (getVals "shoubatsu") ++ [
		End
	 ]
	return ()
	where
	getVal t = let P.Left v = fromJust $ lookup t dat in v
	getVals t = let P.Right vs = fromJust $ lookup t dat in vs

readHists :: [String] -> [History]
readHists = map readHistory

readHistory :: String -> History
readHistory str = case words str of
	["なし"] -> Title "なし"
	[y, m, h] -> History (read y) (read m) h

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

koukou, daigaku :: String
koukou = "東京都立東X高等学校"
daigaku = "東X大学文学部社会心理学科"

data History =
	Title String | History Int Int String | Memo String | End deriving Show

addHist :: (Area, Area, Area) -> History -> AreaM ()
addHist (ya, ma, ba) (History y m b) = do
	addStr ya (Center, Middle) False 85 $ show y
	addStr ma (Center, Middle) False 85 $ show m
	addStr ba (Left, Middle) False 85 b
addHist (_, _, ba) (Title t) = addStr ba (Center, Middle) False 100 t
addHist (_, _, ba) (Memo m) = addStr ba (Left, Middle) False 50 m
addHist (_, _, ba) End = addStr ba (Right, Middle) False 100 "以上"

mkHistArea :: Area -> AreaM (Maybe ((Area, Area, Area), Area))
mkHistArea a0@(Area _ _ _ h)
	| h < 160 = return Nothing
	| otherwise = do
		(a1, rest) <- hSepArea a0 False 160
		(a2, a2_) <- vSepArea a1 True 400
		(a3, a4) <- vSepArea a2_ False 200
		return $ Just ((a2, a3, a4), rest)
