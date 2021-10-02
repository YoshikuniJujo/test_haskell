メモ
====

todo
----

* [x] make type Rgba more general
* [x] Surface
	+ [x] vector
	+ [x] raster
* [x] Transform
	+ [x] copy transformation to Path
	+ [x] remove transformation from Draw
* [ ] Source
	+ consider to remove
* [x] Mask
	+ [x] MaskAlpha
	+ [x] MaskPaint
	+ [x] MaskStroke
		- [x] line width
		- [x] dash
		- [x] line cap
		- [x] line join
			* [x] miter
				+ [x] miter limit
				+ [x] others
			* [x] round
			* [x] bevel
	+ [x] MaskFill
		- [x] fill rule
	+ [x] MaskTextLayout
* [x] Pattern
	+ [x] PatternSolid
	+ [x] PatternNonSolid
		- [x] filter
			* [x] make 128x128 ruster image
			* [x] make 2x2 ruster image pattern
			* [x] others
		- [x] extend
		- [x] matrix
* [ ] PatternNonSolid
	+ [x] PatternSurface
	+ [x] PatternGradientLinear
	+ [x] PatternGradientRadial
	+ [ ] PatternMesh
		- [x] implement
		- [ ] more trials
* [x] Paths
	+ [x] move to
	+ [x] line to
	+ [x] close path
	+ [x] rectangle
	+ [x] arc
	+ [x] curve to
* [x] Clip
	+ [x] multi clip
	+ [x] clip should be more large
	+ [x] add fill rule
* [x] Operator
* [ ] consider to unify `CurveTo`, `LineTo`, `MoveTo`, `CloseTo` and so on

Pattern
------

* Surface
* Color
* Gradient
	+ Linear
	+ Radial
* Mesh - Tensor-product patch mesh

Source
------

* Pattern
* Surface
* Color

Mask
----

* Pattern
* Surface
* Paint
	+ Whole
	+ With Alpha
* Stroke
* Fill
* Glyphs

Surface
-------

* Image
* From Destination

Transformation
--------------
