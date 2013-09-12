import DrawArea
import Tools
import Prelude hiding (Left, Right)
import Control.Monad

main :: IO ()
main = (putStr =<<) . runArea 707 1000 $ do
	mkBaseArea0
	mkBaseArea1
	mkBaseArea2

mkBaseArea0, mkBaseArea1, mkBaseArea2 :: AreaM ()
mkBaseArea0 = do
	baseArea0 <- mkArea 50 80 600 330
	(area0, area1) <- hSepArea baseArea0 False 24
	(year, area2) <- vSepArea area0 True 80
	(month, title) <- vSepArea area2 False 40
	addStr year (Center, Middle) False 12 "年"
	addStr month (Center, Middle) False 12 "月"
	addStr title (Center, Middle) False 12 "免許・資格"
	(area3, tokki) <- hSepArea area1 False 256
	las <- unfoldrM mkLicenseArea area3
	zipWithM addLicense las [
		(1990, 5, "普通自動車第一種運転免許取得"),
		(1990, 9, "TOEIC公開テスト スコア850取得")
	 ]
	addStr tokki (Left, Top) False 12 "その他特記すべき事項"
	addStr tokki (Center, Middle) False 17
		"TOEICスコア 900以上を目指して、現在勉強中"

mkLicenseArea :: Area -> AreaM (Maybe ((Area, Area, Area), Area))
mkLicenseArea a0@(Area _ _ _ h)
	| h < 32 = return Nothing
	| otherwise = do
		(a1, rest) <- hSepArea a0 False 32
		(a2, a2_) <- vSepArea a1 True 80
		(a3, a4) <- vSepArea a2_ False 40
		return $ Just ((a2, a3, a4), rest)

addLicense :: (Area, Area, Area) -> (Int, Int, String) -> AreaM ()
addLicense (ya, ma, la) (y, m, l) = do
	addStr ya (Center, Middle) False 17 $ show y
	addStr ma (Center, Middle) False 17 $ show m
	addStr la (Left, Middle) False 17 l

mkBaseArea1 = do
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
	addMLStr gakkaB (Center, Middle) 12 $
		"ウェブスキル全般。特にウェブ広告に携わって" ++
		"まいりましたので、ウェブの知識は豊富です。"
	addMLStr health (Center, Middle) 12 "きわめて良好"
	addMLStr hobby (Center, Middle) 12 
		"ギャンブルやマニアックな趣味"
	addMLStr sports (Center, Middle) 12 "野球"
	addMLStr douki (Left, Middle) 12 $
		"広告代理店に入社以来、紙媒体やウェブの広告制作、" ++
		"ブランド構築に携わってまいりました。その経験を生かし、" ++
		"ほかのメディアやイベントプロデュースなどさらに大きな" ++
		"フィールドで幅を広げたく、応募いたしました。"
	addStr kibouB (Left, Top) False 12
		"職種: メディアプランナーを希望します。"
	addStr station (Center, Bottom) False 15 "JR 中央駅 目黒駅"
	addStr yaku (Center, Bottom) False 15 "約"
	addStr hour (Center, Bottom) False 15 "1"
	addStr hourU (Center, Bottom) False 15 "時間"
	addStr min (Center, Bottom) False 15 "20"
	addStr minU (Center, Bottom) False 15 "分"
	addStr num (Center, Bottom) False 15 "0"
	addStr numU (Center, Bottom) False 15 "人"
	addStr haigu (Center, Bottom) False 15 "有"
	addStr haigufuyou (Center, Bottom) False 15 "有"

mkBaseArea2 = do
	baseArea2 <- mkArea 50 850 600 100
	addStr baseArea2 (Left, Top) False 12
		"採用者側の記入欄(志望者は記入しないこと)"
