import DrawArea
import Parse
import Tools
import Prelude hiding (Left, Right)
import qualified Prelude as P
import Control.Monad
import Data.Maybe

main :: IO ()
main = do
	dat <- readData "rirekisho.dat"
	(putStr =<<) . runArea 3535 5000 $ do
		mkBaseArea0 dat
		mkBaseArea1 dat
		mkBaseArea2 dat

mkBaseArea0, mkBaseArea1, mkBaseArea2 ::
	[(String, Either String[String])] -> AreaM ()
mkBaseArea0 dat = do
	baseArea0 <- mkArea 250 400 3000 1650
	(area0, area1) <- hSepArea baseArea0 False 120
	(year, area2) <- vSepArea area0 True 400
	(month, title) <- vSepArea area2 False 200
	addStr year (Center, Middle) False 60 "年"
	addStr month (Center, Middle) False 60 "月"
	addStr title (Center, Middle) False 60 "免許・資格"
	(area3, tokki) <- hSepArea area1 False 1280
	las <- unfoldrM mkLicenseArea area3
	zipWithM addLicense las $ map readLicense $ getVals "license" {- [
		(1990, 5, "普通自動車第一種運転免許取得"),
		(1990, 9, "TOEIC公開テスト スコア850取得")
	 ] -}
	addStr tokki (Left, Top) False 60 "その他特記すべき事項"
	addStr tokki (Center, Middle) False 85 $ getVal "tokki"
	where
	getVal t = let P.Left v = fromJust $ lookup t dat in v
	getVals t = let P.Right vs = fromJust $ lookup t dat in vs

mkLicenseArea :: Area -> AreaM (Maybe ((Area, Area, Area), Area))
mkLicenseArea a0@(Area _ _ _ h)
	| h < 160 = return Nothing
	| otherwise = do
		(a1, rest) <- hSepArea a0 False 160
		(a2, a2_) <- vSepArea a1 True 400
		(a3, a4) <- vSepArea a2_ False 200
		return $ Just ((a2, a3, a4), rest)

readLicense :: String -> (Int, Int, String)
readLicense str = let [y, m, l] = words str in (read y, read m, l)

addLicense :: (Area, Area, Area) -> (Int, Int, String) -> AreaM ()
addLicense (ya, ma, la) (y, m, l) = do
	addStr ya (Center, Middle) False 85 $ show y
	addStr ma (Center, Middle) False 85 $ show m
	addStr la (Left, Middle) False 85 l

mkBaseArea1 dat = do
	baseArea1 <- mkArea 250 2100 3000 2100
	(area0, area1) <- hSepArea baseArea1 False 400
	(gakka, health) <- vSepArea area0 False 1500
	(_, gakkaB) <- vSepAreaLog gakka 100
	(area2, area3) <- hSepArea area1 False 400
	(hobby, sports) <- vSepArea area2 False 1500
	(area4, area5) <- hSepArea area3 False 400
	(_, douki) <- vSepAreaLog area4 100
	(kibou, area6) <- vSepArea area5 False 2000
	(_, kibou1) <- hSepAreaLog kibou 75
	(_, kibou2) <- hSepAreaLog kibou1 150
	(_, kibouB) <- vSepAreaLog kibou2 75
	(station, area7) <- hSepArea area6 False 225
	(time, area8) <- hSepArea area7 False 225
	(_, time1) <- vSepAreaLog time 150
	(yaku, time2) <- vSepAreaLog time1 150
	(hour, time3) <- vSepAreaLog time2 150
	(hourU, time4) <- vSepAreaLog time3 225
	(min, minU) <- vSepAreaLog time4 150
	(fuyou, area9) <- hSepArea area8 False 225
	(_, fuyou1) <- vSepAreaLog fuyou 400
	(num, numU) <- vSepAreaLog fuyou1 250
	(haigu, haigufuyou) <- vSepArea area9 False 400
	addStr gakka (Left, Top) False 60 "得意な学科"
	addStr health (Left, Top) False 60 "健康状態"
	addStr hobby (Left, Top) False 60 "趣 味"
	addStr sports (Left, Top) False 60 "スポーツ"
	addStr area4 (Left, Top) False 60 "志望の動機"
	addStr kibou (Left, Top) False 60 "本人希望記入欄"
	addStr kibou1 (Left, Top) False 60
		"(特に給料・職種・勤務時間・勤務地その他について希望があれば記入)"
	addStr station (Left, Top) False 60 "最寄駅"
	addStr time (Left, Top) False 60 "通勤時間"
	addStr fuyou (Left, Top) False 60 "扶養家族(配偶者を除く)"
	addStr haigu (Left, Top) False 60 "配偶者"
	addStr haigufuyou (Left, Top) False 60 "配偶者の扶養義務"
	addMLStr gakkaB (Left, Middle) 60 $ getVal "gakka"
	addMLStr health (Center, Middle) 60 $ getVal "health"
	addMLStr hobby (Center, Middle) 60 $ getVal "hobby"
	addMLStr sports (Center, Middle) 60 $ getVal "sports"
	addMLStr douki (Left, Middle) 60 $ getVal "douki"
	kas <- unfoldrM mkKibouArea kibouB
	zipWithM addKibou kas $ map readKibou $ getVals "kibou"
	addStr station (Center, Bottom) False 75 $ getVal "station" -- "JR 中央駅 目黒駅"
	addStr yaku (Center, Bottom) False 75 "約"
	addStr hour (Center, Bottom) False 75 $ takeWhile (/= ':') $ getVal "time"
	addStr hourU (Center, Bottom) False 75 "時間"
	addStr min (Center, Bottom) False 75 $
		tail $ dropWhile (/= ':') $ getVal "time"
	addStr minU (Center, Bottom) False 75 "分"
	addStr num (Center, Bottom) False 75 $ getVal "fuyou"
	addStr numU (Center, Bottom) False 75 "人"
	addStr haigu (Center, Bottom) False 75 $ getVal "haigu"
	addStr haigufuyou (Center, Bottom) False 75 $ getVal "haiguFuyou"
	where
	getVal t = let P.Left v = fromJust $ lookup t dat in v
	getVals t = let P.Right vs = fromJust $ lookup t dat in vs

readKibou :: String -> (String, String)
readKibou str = let [t, v] = words str in (t, v)

addKibou :: (Area, Area, Area) -> (String, String) -> AreaM ()
addKibou (ta, ca, va) (t, v) = do
	addStr ta (Left, Middle) False 60 t
	addStr ca (Center, Middle) False 60 ":"
	addStr va (Left, Middle) False 60 v

mkKibouArea :: Area -> AreaM (Maybe ((Area, Area, Area), Area))
mkKibouArea a0@(Area _ _ _ h)
	| h < 100 = return Nothing
	| otherwise = do
		(a1, rest) <- hSepAreaLog a0 100
		(a2, a2_) <- vSepAreaLog a1 250
		(a3, a4) <- vSepAreaLog a2_ 30
		return $ Just ((a2, a3, a4), rest)

mkBaseArea2 dat = do
	baseArea2 <- mkArea 250 4250 3000 500
	addStr baseArea2 (Left, Top) False 60
		"採用者側の記入欄(志望者は記入しないこと)"
