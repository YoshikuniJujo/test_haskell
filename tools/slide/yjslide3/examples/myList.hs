myHead :: [a] -> a
myHead (h : _) = h
myHead [] = error "myHead: empty list"

myTail :: [a] -> [a]
myTail (_ : t) = t
