{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

fun :: (forall a . [a] -> [a]) -> [Integer] -> [Char] -> ([Integer], [Char])
fun f ns cs = (f ns, f cs)

foo :: ([a] -> [a]) -> [a] -> [a]
foo f xs = f xs

bar :: (forall a . [a] -> [a]) -> [b] -> [b]
bar f xs = f xs
