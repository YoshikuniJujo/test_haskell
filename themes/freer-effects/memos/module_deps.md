# package dependency

```

						yaftee-condiut-bytestring
							|
							|
							|
	yaftee-basic-monads			yaftee-conduit
		|					|
		+---------------+-----------------------+
				|
				yaftee
				|
			+-------+-----------------------+
			|				|
	higher-order-freer-monad		    higher-order-open-union
			|				|
		+-------+-----------------------+	|
		|				|	|
		ftcqueue		    freer-base-classes

```

# modules

## yaftee-base-monads

* [x] Reader
* [x] Writer
* [x] State
* [x] Except
* [x] NonDet
* [x] Fail
* [x] Trace
* [x] ST
* [ ] IO

## yaftee-conduit

* Pipe

## yaftee-theoreticall

* ReaderI
* WriterO
