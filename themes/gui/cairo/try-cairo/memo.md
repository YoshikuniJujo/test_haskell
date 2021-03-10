memo
====

* [x] make red 100% image in PNG
* [x] make alpha chanel sample in PNG
* [x] define cairoRotate
* [ ] to try to read and write .png file
	+ [ ] read -> convert -> paint -> convert -> write
		- [x] RGBA8 -> Argb32 -> rotate -> Argb32 -> RGBA8
		- [x] Y8 -> A8
		- [ ] others
* [x] move readArgb32 to Parts
* [x] move writeArgb32 to Parts
* [x] try to use surface of A8 as destination
* [x] try to use surface of A1 as destination

backup
------

* [ ] make Black White pattern
* [ ] make converter from R, G, B, A to ARGB
* [ ] make function to pre multiply
* [ ] try over draw
	+ [ ] try no pre multiplied
	+ [ ] try pre multiplied
* [ ] make tool to read PNG and to convert to surphace with JuicyPixels
