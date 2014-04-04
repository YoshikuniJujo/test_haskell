import Data.List

c3 :: [String -> String]
c3 = cycle $ replicate 2 id ++ [(++ "Fizz")]

c5 :: [String -> String]
c5 = cycle $ replicate 4 id ++ [(++ "Buzz")]

cn :: [String -> String]
cn = map (\i s -> if null s then show i else s) [1 ..]

ret = map (($ "") . foldr1 (.)) $ transpose [cn, c5, c3]

main = mapM_ putStrLn $ take 100 ret
