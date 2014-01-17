myHead :: [a] -> a
myHead (h : _) = h
myHead [] = error "myHead: empty list"

myTail :: [a] -> [a]
myTail (_ : t) = t

myFoldl op s [] = s
myFoldl op s (x : xs) = foldl op (s `op` x) xs
