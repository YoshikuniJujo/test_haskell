Visuals memo
============

* [x] make `Graphics.Gdk.Visuals`
* [ ] refactor export list
	+ [x] structure
	+ [x] GDK VISUAL
	+ [x] SCREEN
		- `gdkVisualGetScreen`
	+ [x] GDK VISUAL TYPE
		- [x] `gdkVisualGetVisualType`
		- [x] `data GdkVisualType`
		- [x] patterns
	+ [x] DEPTH
		- `gdkVisualGetDepth`
	+ [ ] RED, GREEN AND BLUE
		- [x] `gdkVisualGetRedPixelDetails`
		- [ ] `gdkVisualGetGreenPixelDetails`
		- [ ] `gdkVisualBluePixelDetails`

Function
--------

### now

* [x] `gdk_visual_get_depth`
* [x] `gdk_visual_get_red_pixel_details`
* [x] `gdk_visual_get_green_pixel_details`
* [x] `gdk_visual_get_blue_pixel_detauls`
* [x] `gdk_visual_get_visual_type`
* [x] `gdk_visual_get_screen`

### deprecated

* `gdk_query_depth`
* `gdk_query_visual_types`
* `gdk_list_visuals`
* `gdk_visual_get_bits_per_rgb`
* `gdk_visual_get_byte_order`
* `gdk_visual_get_colormap_size`
* `gdk_visual_get_best_depth`
* `gdk_visual_get_best_type`
* `gdk_visual_get_system`
* `gdk_visual_get_best`
* `gdk_visual_get_best_with_depth`
* `gdk_visual_get_best_with_type`
* `gdk_visual_get_best_with_both`

todo
----

* [x] add export list
* [x] move `GdkVisual` to `Graphics.Gdk.Visuals` from `Types`
