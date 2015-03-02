myHead :: [a] -> a
myHead (x : _) = x

myTail :: [a] -> [a]
myTail (_ : xs) = xs
