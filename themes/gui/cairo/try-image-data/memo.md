メモ
====

todo
----

* [x] make type Rgba more general
* [x] Surface
	+ [x] vector
	+ [x] raster
* [ ] Transform
	+ [x] copy transformation to Path
	+ [ ] copy transformation to Gradient
	+ [ ] copy transformation to Mesh
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
* [ ] Alpha
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
	+ [ ] PatternGradientLinear
	+ [ ] PatternGradientRadial
	+ [ ] PatternMesh
* [ ] Paths
	+ [x] move to
	+ [x] line to
	+ [x] close path
	+ [x] rectangle
	+ [x] arc
	+ [ ] others
* [x] Clip
	+ [x] multi clip
	+ [x] clip should be more large
	+ [x] add fill rule
* [x] Operator

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
