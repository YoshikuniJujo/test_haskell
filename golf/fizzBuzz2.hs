[c3, c5] = zipWith (\n s -> cycle $ replicate n id ++ [(++ s)]) [2, 4] ["Fizz", "Buzz"]

cn' = map (\i s -> [s,show i]!!fromEnum(null s)) [1 ..]
cn = map (\i s -> if null s then show i else s) [1 ..]

main = mapM_ putStrLn $ take 100 $ zipWith3 (\f g h -> f $ g $ h "") cn c5 c3
