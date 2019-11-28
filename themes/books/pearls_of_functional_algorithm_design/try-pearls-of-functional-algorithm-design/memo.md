memo
====

Ch. 15 All the common prefixes
------------------------------

### samples

```haskell
p = llcp xs $ drop i xs
q = llcp xs $ drop j xs
```

```haskell
j <= p
```

### q /= p - j

```
	...q...j.....p.....i......k.............
orig	abcdefgabcdlmnxxxxxabcdefgabcdlmnopqrstu
from i	abcdefgabcdlmnopqrstu
from j 	abcdlmnxxxxxabcdefgabcdlmnopqrstu

k = i + j
```

```
	...q...j......p....i......k.............
orig	abcdefghijklmnxxxxxabcdefgabcdlmnopqrstu
from j	abcdlmnxxxxxabcdefgabcdlmnopqrstu
from k	abcdlmnopqrstu
```

### q == p - j

```
	...q...j..p........i......k.............
orig	abcdefgabcdxxxxxxxxabcdefgabcdlmnopqrstu
from i	abcdefgabcdlmnopqrstu
from j 	abcdxxxxxxxxabcdefgabcdlmnopqrstu

k = i + j
```

```
	...q...j..p........i......k.............
orig	abcdefgabcdxxxxxxxxabcdefgabcdlmnopqrstu
from j	abcdxxxxxxxxabcdefgabcdlmnopqrstu
from k	abcdlmnopqrstu
```

### basic

```
p = llcp xs (drop i xs)
q = llcp xs (drop j xs)
j <= p
k = i + j

q = llcp xs (drop j xs)
p - j = llcp (drop j xs) (drop k xs)

llcp xs (drop k xs)
	| q /= p - j = min (p - j) q
	| q == p - j = q + llcp (drop q xs) (drop (q + k) xs)
```

### code

Ch. 16 The Boyer Moore algorithm
---------------------------------

### scan lemma and foldl tupling law

```haskell
map (foldl op e) . inits = scanl op e

map f . filter p = map fst . filter snd . map (f &&& p)
map f . filter (p . g) = map fst . filter (p . snd) . map (f &&& g)

op (a, b) = op1 a &&& op2 b => foldl op1 e1 &&& foldl op2 e2 = foldl op (e1, e2)
```

```haskell
matches ws =
	map length . filter (endswith ws) . inits
		|
		V
	map fst . filter snd . map (length &&& endswith ws) . inits
		|
		V
	endswith ws = foldl op e
	let step (n, x) y = (n + 1, op x y) in
		map fst . filter snd . map (foldl step (0, e)) . inits
		|
		V
	map fst . filter snd . scanl step (0, e)
```

```haskell
	endswith ws = p . foldl op e
	.
	.
	.
matches ws = map fst . filter (p . snd) . scanl step (0, e)
```

```haskell
endswith ws = (reverse ws `isPrefixOf`) . reverse
```

```haskell
matches ws =
	map length . filter (endswith ws) . inits
		|
		V
	map length . filter ((reverse ws `isPrefixOf`) . reverse) . inits
		|
		V
	map fst . filter ((sw `isPrefixOf`) . snd)
		. map (length &&& reverse) . inits
	where sw = reverse ws
		|
	(reverse = foldl (flip (:)) [])
		|
		V
	map fst . filter ((sw `isPrefixOf`) . snd) . scanl step (0, [])
	where step (n, sx) x = (n + 1, x : sx)
```

### shift

```
i = llcp sw sx

k = length ys
(n + k, ys ++ sx)

sw `isPrefixOf` ys ++ sx
	=> take k sw == ys && drop k sw `isPrefixOf` sx
	=> llcp sw (drop k sw) = min i (m - k)
```

```
take i (drop k sw)
		|
	(drop k sw `isPrefixOf` sx => drop k sw = take (m - k) sx)
		|
		V
	take i (take (m - k) sx)
		|
		V
	take i sx
		|
		V
	take i sw
```

```
	1.....m....n...j..........
text	abcdefghijopmnopqrstuvwxyz
ws	jopmnop

j = n + k
m = 7
k = 4
```

```
sx	pojihgfedcba
sw	ponmpoj

i = llcp sw sx = 2
```

```
ys++sx	ponmpojihgfedcba
sw	ponmpoj
sw	    ponmpoj
```

```
take k sw	ponm
ys		ponm

sx		pojhgfedcba
drop k sw	poj
```

```
sw		ponmpoj
drop k sw	poj
```
