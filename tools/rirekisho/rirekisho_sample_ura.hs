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
	(putStr =<<) . runArea 707 1000 $ do
		mkBaseArea0 dat
		mkBaseArea1 dat
		mkBaseArea2 dat

mkBaseArea0, mkBaseArea1, mkBaseArea2 ::
	[(String, Either String[String])] -> AreaM ()
mkBaseArea0 dat = do
	baseArea0 <- mkArea 50 80 600 330
	(area0, area1) <- hSepArea baseArea0 False 24
	(year, area2) <- vSepArea area0 True 80
	(month, title) <- vSepArea area2 False 40
	addStr year (Center, Middle) False 12 "年"
	addStr month (Center, Middle) False 12 "月"
	addStr title (Center, Middle) False 12 "免許・資格"
	(area3, tokki) <- hSepArea area1 False 256
	las <- unfoldrM mkLicenseArea area3
	zipWithM addLicense las $ map readLicense $ getVals "license" {- [
		(1990, 5, "普通自動車第一種運転免許取得"),
		(1990, 9, "TOEIC公開テスト スコア850取得")
	 ] -}
	addStr tokki (Left, Top) False 12 "その他特記すべき事項"
	addStr tokki (Center, Middle) False 17 $ getVal "tokki"
	where
	getVal t = let P.Left v = fromJust $ lookup t dat in v
	getVals t = let P.Right vs = fromJust $ lookup t dat in vs

mkLicenseArea :: Area -> AreaM (Maybe ((Area, Area, Area), Area))
mkLicenseArea a0@(Area _ _ _ h)
	| h < 32 = return Nothing
	| otherwise = do
		(a1, rest) <- hSepArea a0 False 32
		(a2, a2_) <- vSepArea a1 True 80
		(a3, a4) <- vSepArea a2_ False 40
		return $ Just ((a2, a3, a4), rest)

readLicense :: String -> (Int, Int, String)
readLicense str = let [y, m, l] = words str in (read y, read m, l)

addLicense :: (Area, Area, Area) -> (Int, Int, String) -> AreaM ()
addLicense (ya, ma, la) (y, m, l) = do
	addStr ya (Center, Middle) False 17 $ show y
	addStr ma (Center, Middle) False 17 $ show m
	addStr la (Left, Middle) False 17 l

mkBaseArea1 dat = do
	baseArea1 <- mkArea 50 420 600 420
	(area0, area1) <- hSepArea baseArea1 False 80
	(gakka, health) <- vSepArea area0 False 300
	(_, gakkaB) <- vSepAreaLog gakka 20
	(area2, area3) <- hSepArea area1 False 80
	(hobby, sports) <- vSepArea area2 False 300
	(area4, area5) <- hSepArea area3 False 80
	(_, douki) <- vSepAreaLog area4 20
	(kibou, area6) <- vSepArea area5 False 400
	(_, kibou1) <- hSepAreaLog kibou 15
	(_, kibou2) <- hSepAreaLog kibou1 30
	(_, kibouB) <- vSepAreaLog kibou2 15
	(station, area7) <- hSepArea area6 False 45
	(time, area8) <- hSepArea area7 False 45
	(_, time1) <- vSepAreaLog time 30
	(yaku, time2) <- vSepAreaLog time1 30
	(hour, time3) <- vSepAreaLog time2 30
	(hourU, time4) <- vSepAreaLog time3 45
	(min, minU) <- vSepAreaLog time4 30
	(fuyou, area9) <- hSepArea area8 False 45
	(_, fuyou1) <- vSepAreaLog fuyou 80
	(num, numU) <- vSepAreaLog fuyou1 50
	(haigu, haigufuyou) <- vSepArea area9 False 80
	addStr gakka (Left, Top) False 12 "得意な学科"
	addStr health (Left, Top) False 12 "健康状態"
	addStr hobby (Left, Top) False 12 "趣 味"
	addStr sports (Left, Top) False 12 "スポーツ"
	addStr area4 (Left, Top) False 12 "志望の動機"
	addStr kibou (Left, Top) False 12 "本人希望記入欄"
	addStr kibou1 (Left, Top) False 12
		"(特に給料・職種・勤務時間・勤務地その他について希望があれば記入)"
	addStr station (Left, Top) False 12 "最寄駅"
	addStr time (Left, Top) False 12 "通勤時間"
	addStr fuyou (Left, Top) False 12 "扶養家族(配偶者を除く)"
	addStr haigu (Left, Top) False 12 "配偶者"
	addStr haigufuyou (Left, Top) False 12 "配偶者の扶養義務"
	addMLStr gakkaB (Left, Middle) 12 $ getVal "gakka"
	addMLStr health (Center, Middle) 12 $ getVal "health"
	addMLStr hobby (Center, Middle) 12 $ getVal "hobby"
	addMLStr sports (Center, Middle) 12 $ getVal "sports"
	addMLStr douki (Left, Middle) 12 $ getVal "douki"
	kas <- unfoldrM mkKibouArea kibouB
	zipWithM addKibou kas $ map readKibou $ getVals "kibou"
	addStr station (Center, Bottom) False 15 $ getVal "station" -- "JR 中央駅 目黒駅"
	addStr yaku (Center, Bottom) False 15 "約"
	addStr hour (Center, Bottom) False 15 $ takeWhile (/= ':') $ getVal "time"
	addStr hourU (Center, Bottom) False 15 "時間"
	addStr min (Center, Bottom) False 15 $
		tail $ dropWhile (/= ':') $ getVal "time"
	addStr minU (Center, Bottom) False 15 "分"
	addStr num (Center, Bottom) False 15 $ getVal "fuyou"
	addStr numU (Center, Bottom) False 15 "人"
	addStr haigu (Center, Bottom) False 15 $ getVal "haigu"
	addStr haigufuyou (Center, Bottom) False 15 $ getVal "haiguFuyou"
	where
	getVal t = let P.Left v = fromJust $ lookup t dat in v
	getVals t = let P.Right vs = fromJust $ lookup t dat in vs

readKibou :: String -> (String, String)
readKibou str = let [t, v] = words str in (t, v)

addKibou :: (Area, Area, Area) -> (String, String) -> AreaM ()
addKibou (ta, ca, va) (t, v) = do
	addStr ta (Left, Middle) False 12 t
	addStr ca (Center, Middle) False 12 ":"
	addStr va (Left, Middle) False 12 v

mkKibouArea :: Area -> AreaM (Maybe ((Area, Area, Area), Area))
mkKibouArea a0@(Area _ _ _ h)
	| h < 20 = return Nothing
	| otherwise = do
		(a1, rest) <- hSepAreaLog a0 20
		(a2, a2_) <- vSepAreaLog a1 50
		(a3, a4) <- vSepAreaLog a2_ 6
		return $ Just ((a2, a3, a4), rest)

mkBaseArea2 dat = do
	baseArea2 <- mkArea 50 850 600 100
	addStr baseArea2 (Left, Top) False 12
		"採用者側の記入欄(志望者は記入しないこと)"
