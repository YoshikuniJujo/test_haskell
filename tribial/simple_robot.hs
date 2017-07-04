{-# LANGUAGE Rank2Types #-}

fun :: (forall a . a -> a) -> Char -> Int -> (Char, Int)
fun f c n = (f c, f n)
