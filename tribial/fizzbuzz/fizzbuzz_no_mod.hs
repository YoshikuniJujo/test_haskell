fizz, buzz :: [String]
fizz = ["", "", "Fizz"]
buzz = ["", "", "", "", "Buzz"]

fizzbuzz :: [String]
fizzbuzz = map process . zip [1..] $ zip (cycle fizz) (cycle buzz)

process :: (Int, (String, String)) -> String
process (n, ("", "")) = show n
process (_, (f, b)) = f ++ b
