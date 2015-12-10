import Control.Arrow
import Data.Maybe
import Data.List
import Data.Function
import Data.Time
import GHC.IO.Encoding

enoughTime :: DiffTime
enoughTime = 3 * 60 * 60

ijou :: Ord a => a -> a -> Bool
ijou = (>=)
-- ijou = (>)

morning, night :: TimeOfDay
morning = TimeOfDay 6 0 0
night = TimeOfDay 22 0 0

main :: IO ()
main = do
	setLocaleEncoding char8
	interact $ format . head . filter ((`ijou` enoughTime) . period . snd)
		. map maxPeriod . uncurry sync
		. (fromMaybe [] . lookup "boy" &&& fromMaybe [] . lookup "girl")
		. map (second frees) . parse

format :: (Day, Period) -> String
format (ymd, (b, e)) = dt ++ "," ++ tm b ++ "," ++ tm e ++ "\n"
	where
	(y, m, d) = toGregorian ymd
	dt = show y ++ "/" ++ pad (show m) ++ "/" ++ pad (show d)
	tm t = pad (show $ todHour t) ++ ":" ++ pad (show $ todMin t)
	pad s = replicate (2 - length s) '0' ++ s

type Frees = [Free]

frees :: Schedule -> Frees
frees sc@(Event d0 _ _ : _) = (`Free` [(morning, night)]) `map` [d0 ..] `sb`
	grouping ((eDate &&& id) `map` sc)
	where
	[] `sb` _ = []
	fa `sb` [] = fa
	fa@(f@(Free fd _) : fs) `sb` ea@((ed, evs) : es)
		| fd < ed = fs `sb` ea
		| fd > ed = fa `sb` es
		| otherwise = foldl sub f evs : fs `sb` es
frees _ = []

sync :: Frees -> Frees -> Frees
[] `sync` _ = []
_ `sync` [] = []
fa1@(f1@(Free d1 _) : fs1) `sync` fa2@(f2@(Free d2 _) : fs2)
	| d1 < d2 = fs1 `sync` fa2
	| d1 > d2 = fa1 `sync` fs2
	| otherwise = f1 `inter` f2 : sync fs1 fs2

data Free = Free { fDay :: Day, fPeriods :: [Period] } deriving (Show, Eq, Ord)

maxPeriod :: Free -> (Day, Period)
maxPeriod (Free d ps) = (d, maximumBy (compare `on` period) ps)

sub :: Free -> Event -> Free
Free fd ps0 `sub` Event ed (eb, ee) _
		| fd /= ed = error "sub: different days"
		| eb >= ee = error "sub: begin time should be before end time"
		| otherwise = Free fd $ sb ps0
	where
	sb pa@((b, e) : ps)
		| b >= e = error "sub: begin time should be before end time"
		| ee <= b = pa
		| eb <= b && ee < e = (ee, e) : ps
		| eb <= b = sb ps
		| ee < e = (b, eb) : (ee, e) : ps
		| eb < e = (b, eb) : sb ps
		| otherwise = (b, e) : sb ps
	sb [] = []

inter :: Free -> Free -> Free
Free d ps0 `inter` Free d' ps0'
	| d == d' = Free d $ ps0 `it` ps0'
	| otherwise = error "inter: different days"
	where
	[] `it` _ = []
	_ `it` [] = []
	pa1@((b1, e1) : ps1) `it` pa2@((b2, e2) : ps2)
		| b1 >= e1 || b2 >= e2 =
			error "inter: begin time should be before end time"
		| e2 <= b1 = pa1 `it` ps2
		| b2 < b1 && e2 < e1 = (b1, e2) : pa1 `it` ps2
		| b2 < b1 = (b1, e1) : ps1 `it` pa2
		| e2 < e1 = (b2, e2) : pa1 `it` ps2
		| b2 < e1 = (b2, e1) : ps1 `it` pa2
		| otherwise = ps1 `it` pa2

type Who = String
type Schedule = [Event]
data Event = Event { eDate :: Day, ePeriod :: Period, eTitle :: String }
	deriving (Show, Eq, Ord)

parse :: String -> [(Who, Schedule)]
parse = grouping . sort . map (r . sp ',') . lines
	where
	r [wh, dt, bg, ed, ttl] = (wh, Event (rd dt) (rt bg, rt ed) ttl)
	r _ = error "parse: bad event format"
	rd s = case sp '/' s of
		[y, m, d] -> fromGregorian (read y) (read m) (read d)
		_ -> error "parse: bad date format"
	rt s = case sp ':' s of
		[h, m] -> TimeOfDay (read h) (read m) 0
		_ -> error "parse: bad time format"
	sp p s = case span (/= p) s of (_, "") -> [s]; (h, _ : t) -> h : sp p t

type Period = (TimeOfDay, TimeOfDay)

period :: Period -> DiffTime
period (b, e) = timeOfDayToTime e - timeOfDayToTime b

grouping :: Eq a => [(a, b)] -> [(a, [b])]
grouping = map (fst . head &&& map snd) . groupBy ((==) `on` fst)
