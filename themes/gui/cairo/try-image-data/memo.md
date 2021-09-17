メモ
====

todo
----

* [ ] make type Rgba more general
* [ ] Surface
* [ ] Transform
	+ [x] copy transformation to Path
	+ [ ] copy transformation to Gradient
	+ [ ] copy transformation to Mesh
	+ [x] remove transformation from Draw
* [ ] Source
* [ ] Mask
	+ [ ] MaskAlpha
	+ [x] MaskPaint
	+ [ ] MaskStroke
	+ [x] MaskFill
	+ [ ] MaskGlyphs
* [ ] Alpha
* [ ] Pattern
	+ [ ] PatternSurface
	+ [x] PatternColor
	+ [ ] PatternGradient
	+ [ ] PatternMesh
* [ ] Paths
	+ [x] rectangle
	+ [x] arc
	+ [x] line width
	+ [ ] dash
	+ [ ] fill rule
	+ [ ] line cap
	+ [x] line join
		- [x] miter
			* [x] miter limit
			* [x] others
		- [x] round
		- [x] bevel
	+ [ ] others
* [ ] Clip
	+ [x] multi clip
	+ [x] clip should be more large
	+ [ ] add fill rule
* [ ] Operator

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
