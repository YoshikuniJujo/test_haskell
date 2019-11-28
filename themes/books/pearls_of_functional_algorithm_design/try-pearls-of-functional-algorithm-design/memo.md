memo
====

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
