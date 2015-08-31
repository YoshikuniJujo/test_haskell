fibs, tfibs :: [Integer]
fibs@(_ : tfibs) = 0 : 1 : zipWith (+) fibs tfibs
