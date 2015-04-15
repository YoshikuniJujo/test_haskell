main :: IO ()
main = interact $ unwords . numberToWords . read

numberToWords :: Integer -> [String]
numberToWords 0 = []
numberToWords 1 = ["one"]
numberToWords 2 = ["two"]
numberToWords 3 = ["three"]
numberToWords 4 = ["four"]
numberToWords 6 = ["six"]
numberToWords 7 = ["seven"]
numberToWords 9 = ["nine"]
numberToWords 13 = ["thirteen"]
numberToWords n | 20 <= n && n < 30 = "twenty" : numberToWords (n - 20)
numberToWords n | 50 <= n && n < 60 = "fifty" : numberToWords (n - 50)
numberToWords n | 80 <= n && n < 90 = "eighty" : numberToWords (n - 80)
numberToWords n | 100 <= n && n < 1000 =
	numberToWords (n `div` 100) ++ ["hundred"] ++ numberToWords (n `mod` 100)
numberToWords n | 1000 <= n && n < 1000000 =
	numberToWords (n `div` 1000) ++ ["thousand"] ++ numberToWords (n `mod` 1000)
numberToWords n | 1000000 <= n && n < 1000000000 =
	numberToWords (n `div` 1000000) ++ ["million"] ++ numberToWords (n `mod` 1000000)
