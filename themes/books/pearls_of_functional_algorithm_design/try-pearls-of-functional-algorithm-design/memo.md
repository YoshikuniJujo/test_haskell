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

save comparison
---------------

```
i = llcp sw sx
k = shift sw i
m - k <= i
```

### CASE 1

```
text	qponmlkjihgfedcbahijklmnopqrstuvwxyz
```

```
	.....i.......m...
sx	abcdefghijklmnopq
sw	abcdefabcdefab

m = 14
i = 6
k = 12
m - k = 2
```

```
		..............
sw		abcdefabcdefab
drop k sw	ab
sx		abcdefghijklmnopqrstuvwxyz
```

```
ys ++ sx	srqponmlkjihabcdefghijklmnopq
sw		abcdefabcdefab
```

### CASE 2

```
text	abcabcabcabcabcabcabc
ws	abcabcabc
```

```
	...................
sx	cbacbacba
sw	cbacbacba

m = 9
i = 9
k = 3
m - k = 6
```

```
ys ++ sx	cbacbacbacba
ws		cbacbacba
```

### shifts

```
f k = llcp sw $ drop k sw

f m = 0
f k <= m - k
```

```
i <= m - k <==> k <= m - i
i >  m - k <==> k >  m - i
```

```
shift sw i =
	head [ k | k <- [1 .. m], f k == min i (m - k) ] =
	head (	[ k | k <- [1 .. m - i], f k == i ] ++
		[ k | k <- [m - i + 1 .. m], f k + k == m ] )
	head (	[ k | k <- [1 .. m], f k == i ] ++
		[ k | k <- [m - i + 1 .. m], f k + k == m ] )
```

```
(accumArray op e (0, m) vks) ! i = foldl op e [ k | (v, k) <- vks, v == i ]
```

```
a = accumArray min m (0, m) vks
vks = [ (f k, k) | k <- [1 .. m] ]

a ! i =
	(accumArray min m (0, m) vks) ! i =
	foldl min m [ j | (v, j) <- vks, v == i ] =
	foldl min m [ j | (v, j) <- [ (f k, k) | k <- [1 .. m] ], v == i ] =
	foldl min m [ j | j <- [1 .. m], f j == i ]
	head ([ k | k <- [1 .. m], f k == i ] ++ [m])
```

```haskell
a = accumArray min m (0, m) (vks ++ vks')

reverse vks' =
	[ (i, head [ k | k <- [m - i + 1 .. m], f k + k == m]) | i <- [1 .. m] ] =
	reverse [ (i, head [ k | k <- [m - i + 1 .. m], f k + k == m]) | i <- [m, m - 1 .. 1] ]

vks' =
	[ (i, head [ k | k <- [m - i + 1 .. m], f k + k == m ]) | i <- [m, m - 1 .. 1 ] ] =
	zip [m, m - 1 .. 1] [ head [ k | k <- [m - i + 1 .. m], f k + k == m ] | i <- [m, m - 1 .. 1 ] ] =
	zip [m, m - 1 .. 1] [ head [ k | k <- [j .. m], f k + k == m ] | j <- [1 .. m] ]

head [ k | k <- [j .. m], f k + k == m ]
	= if f j ++ j == m then j else head [ k | k <- [j + 1 .. m], f k + k == m ]
f m = m
f j = if f j + j == m then j else f (j + 1)
	

vks' = zip [m, m - 1, .. 1] (foldr op [] vks)
op (v, k) ks@(~(k' : _)) = if v + k == m then k : ks else k' : ks
```

```
xs = foldr op [] vks

f	1 0 2 5 1 0 2 1 0
k	1 2 3 4 5 6 7 8 9
xs	4 4 4 4 7 7 7 8 9
```

```
allcp' xs = tail (allcp xs) ++ [0]

[ (f k, k) | k <- [1 .. m] ] =
	[ (llcp sw (drop k sw), k) | k <- [1 .. m] ] =
	zip [llcp sw (drop k sw) | k <- [1 .. m]] [1 .. m] =
	zip (allcp' sw) [1 .. m]
```
