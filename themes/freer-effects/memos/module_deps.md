# package dependency

```

						yaftee-condiut-bytestring
							|
							|
							|
	yaftee-base-monads			yaftee-conduit
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
* [ ] Except
* [ ] NonDet
* [ ] Fail
* [ ] Trace
* [ ] ST
* [ ] IO

## yaftee-conduit

* Pipe

## yaftee-theoreticall

* ReaderI
* WriterO
