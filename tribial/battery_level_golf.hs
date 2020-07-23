import Control.Concurrent

main = do
	run1
	threadDelay 10000000
	main

fp = "/sys/class/power_supply/BAT0/charge_"

run1 = do
	n <- read <$> readFile (fp ++ "now")
	f <- read <$> readFile (fp ++ "full")
	putStrLn $ '[' : graph 74 (n / f) ++ "] " ++
		show (round $ n / f * 100 :: Int) ++ "%"

graph n d =
	a `replicate` '*' ++ (n - a) `replicate` ' '
	where a = round (fromIntegral n * d)
