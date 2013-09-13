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
	svg <- runArea 707 1000 $ do
		mkBaseArea0 dat
		mkBaseArea1 dat
	putStr svg

mkBaseArea0 :: [(String, Either String [String])] -> AreaM ()
mkBaseArea0 dat = do
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
	(maddrT, maddr) <- vSepArea area7_2 False 90
	(area8, area9) <- hSepArea area7 False 40
	(paddrT, area10) <- vSepArea area8 False 80
	(paddr, area11) <- vSepArea area10 False 160
	(contactT, contact) <- vSepArea area11 False 90
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
	addStr nameR3 (Left, Middle) False 10 $ getVal "miyojiRubi"
	addStr name3 (Left, Middle) True 35 $ getVal "miyoji"
	addStr nameR4 (Left, Middle) False 10 $ getVal "namaeRubi"
	addStr name4 (Left, Middle) True 35 $ getVal "namae"
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
	let [y, m, d, a] = getVals "birth"
	addStr year (Center, Middle) False 17 y
	addStr yearU (Center, Middle) False 17 "年"
	addStr month (Center, Middle) False 17 m
	addStr monthU (Center, Middle) False 17 "月"
	addStr day (Center, Middle) False 17 d
	addStr dayU (Center, Middle) False 17 "日生"
	addStr age (Left, Middle) False 17 "(満"
	addStr age (Center, Middle) False 17 a
	addStr age (Right, Middle) False 17 "歳)"
	addStr sex (Left, Middle) False 10 "性別"
	addStr sex (Center, Middle) False 17 $ getVal "sex"
	addStr addrRubi (Left, Middle) False 10 "ふりがな"
	addStr addrR1 (Center, Middle) False 10 $ getVal "addressRubi"
	addStr addr (Left, Top) False 10 "現 住 所"
	addStr post1 (Left, Top) False 12 "〒("
--	addStr post2 (Center, Top) False 12 "000-0145"
	addStr post2_1 (Center, Top) False 12 $ getVal "ynum1"
	addStr post2_2 (Center, Top) False 12 "-"
	addStr post2_3 (Center, Top) False 12 $ getVal "ynum2"
	addStr post3 (Right, Top) False 12 ")"
	addStr addr1 (Center, Middle) False 14 $ getVal "address"
	addStr tel (Left, Top) False 10 "TEL"
	addStr tel1 (Center, Middle) False 17 $ getVal "tel"
	addStr fax (Left, Top) False 10 "FAX"
	addStr fax1 (Center, Middle) False 17 $ getVal "fax"
	addStr phoneT (Center, Middle) False 15 "携帯電話"
	addStr phone (Center, Middle) False 17 $ getVal "pphone"
	addStr maddrT (Center, Middle) False 15 "Email"
	addStr maddr (Center, Middle) False 17 $ getVal "email"
	addStr paddrT1 (Center, Middle) False 15 "携帯電話"
	addStr paddrT2 (Center, Middle) False 10 "メールアドレス"
	addStr paddr (Center, Middle) False 12 $ getVal "pemail"
	addStr contactT1 (Left, Middle) False 10 "都合の良い"
	addStr contactT2 (Left, Top) False 10 "連絡方法と時間帯"
	addStr contact1 (Left, Middle) False 10 $ getVal "tocontactme1"
	addStr contact2 (Left, Top) False 10 $ getVal "tocontactme2"
	addStr contRubi (Left, Middle) False 10 "ふりがな"
	addStr cTel (Left, Top) False 10 "TEL"
	addStr cFax (Left, Top) False 10 "FAX"
	addStr cont (Left, Top) False 10 "連 絡 先"
	addStr cpost1 (Left, Top) False 12 "〒("
	addStr cpost2_2 (Center, Top) False 12 "-"
	addStr cpost3 (Right, Top) False 12 ")"
	addStr cont2 (Center, Top) False 12
		"(現住所以外に連絡を希望する場合のみ記入)"
	where
	getVal t = let P.Left v = fromJust $ lookup t dat in v
	getVals t = let P.Right vs = fromJust $ lookup t dat in vs

mkBaseArea1 :: [(String, Either String [String])] -> AreaM ()
mkBaseArea1 dat = do
	baseArea1 <- mkArea 50 380 600 570
	(area0, area1) <- hSepArea baseArea1 False 24
	(year, area2) <- vSepArea area0 True 80
	(month, title) <- vSepArea area2 False 40
	addStr year (Center, Middle) False 12 "年"
	addStr month (Center, Middle) False 12 "月"
	addStr title (Center, Middle) False 12
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
	addStr ya (Center, Middle) False 17 $ show y
	addStr ma (Center, Middle) False 17 $ show m
	addStr ba (Left, Middle) False 17 b
addHist (_, _, ba) (Title t) = addStr ba (Center, Middle) False 20 t
addHist (_, _, ba) (Memo m) = addStr ba (Left, Middle) False 10 m
addHist (_, _, ba) End = addStr ba (Right, Middle) False 20 "以上"

mkHistArea :: Area -> AreaM (Maybe ((Area, Area, Area), Area))
mkHistArea a0@(Area _ _ _ h)
	| h < 32 = return Nothing
	| otherwise = do
		(a1, rest) <- hSepArea a0 False 32
		(a2, a2_) <- vSepArea a1 True 80
		(a3, a4) <- vSepArea a2_ False 40
		return $ Just ((a2, a3, a4), rest)
