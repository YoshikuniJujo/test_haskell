memo
====

todo
----

* [ ] define functions of Fonts
	+ [x] pango\_font\_description\_new
	+ [x] pango\_font\_description\_copy
	+ [x] pango\_font\_description\_copy\_static
	+ no pango\_font\_description\_hash
	+ [x] pango\_font\_description\_equal
	+ [x] pango\_font\_description\_free
		- use in newForeignPtr
	+ no pango\_font\_descriptions\_free
	+ [x] pango\_font\_description\_set\_family
	+ [x] pango\_font\_description\_set\_family\_static
	+ [x] pango\_font\_description\_get\_family
	+ [x] pango\_font\_description\_set\_style
	+ [x] pango\_font\_description\_get\_style
	+ [x] pango\_font\_description\_set\_variant
	+ [x] pango\_font\_description\_get\_variant
	+ [x] pango\_font\_description\_set\_weight
	+ [x] pango\_font\_description\_get\_weight
	+ [x] pango\_font\_description\_set\_stretch
	+ [x] pango\_font\_description\_get\_stretch
	+ [ ] pango\_font\_description\_set\_size
	+ [ ] pango\_font\_description\_get\_size
	+ [ ] pango\_font\_description\_set\_absolute\_size
	+ [ ] pango\_font\_description\_get\_size\_is\_absolute
	+ [ ] pango\_font\_description\_set\_gravity
	+ [ ] pango\_font\_description\_get\_gravity
	+ no pango\_font\_description\_set\_variations
	+ no pango\_font\_description\_set\_variations\_static
	+ no pango\_font\_description\_get\_variations
	+ [ ] pango\_font\_description\_get\_set\_fields
	+ [ ] pango\_font\_description\_unset\_fields
	+ [ ] pango\_font\_description\_merge
	+ [ ] pango\_font\_description\_merge\_static
	+ [ ] pango\_font\_description\_better\_match
	+ [ ] pango\_font\_description\_from\_string
	+ [ ] pango\_font\_description\_to\_string
	+ [ ] pango\_font\_description\_to\_filename
	+ [ ] others

font description
----------------

* family
* style
* variant
* weight
* stretch
* size
* gravity
* variations

### family

* sans, sans serif, sans-serif
* serif
* monospace
* cursive
* fantasy
* Liberation Sans
* LIberation Serif
* Liberation Mono
* Sazanami Mincho
* Sazanami Gothic
* etc

### size

* thousandths of a point
* xx-small
* x-small
* small
* medium
* large
* x-large
* xx-large
* smaller
* larger

### style

* normal
* italic
* oblique

### weight

* ultralight
* light
* normal
* bold
* ultrabold
* heavy
* or numeric

### variant

* normal
* smallcaps

### stretch

* ultracondensed
* extracondensed
* condensed
* semicondensed
* normaal
* semiexpanded
* expanded
* extraexpanded
* ultraexpanded

### gravity

### variations
