GdkEventMask memo
=================

* [x] refactor export list
	+ structure
* [x] make `Graphics.Gdk.Windows.GdkEventMask.Internal`
* [x] refactor export list
	+ [x] structure
	+ [x] GDK EVENT MASK MULTIPLE BITS
		- [x] `data GdkEventMaskMultiBits`
		- [x] `gdkEventMaskMultiBits`
		- [x] patterns
			* [x] `GdkZeroEventsMask`
			* [x] `GdkAllEventsMask`
	+ [x] GDK EVENT MASK SINGLE BIT
		- [x] `data GdkEventMaskSingleBit`
		- [x] `gdkEventMaskSingleBitList`
		- [x] pattens
			* `GdkExposureMask`: no check
			* [x] `GdkPointerMotionMask`
			* [x] `GdkButtonMotionMask`
			* [x] `GdkButton1MotionMask`
			* [x] `GdkButton2MotionMask`
			* [x] `GdkButton3MotionMask`
			* [x] `GdkButtonPressMask`
			* [x] `GdkButtonReleaseMask`
			* [x] `GdkKeyPressMask`
			* [x] `GdkKeyReleaseMask`
			* [x] `GdkEnterNotifyMask`
			* [x] `GdkLeaveNotifyMask`
			* [x] `GdkFocusChangeMask`
			* [x] `GdkStructureMask`
			* [x] `GdkPropertyChangeMask`
			* [x] `GdkVisibilityNotifyMask`
			* `GdkProximityInMask`: no check
			* `GdkProximityOutMask`: no check
			* `GdkSubstructureMask`: no check
			* [x] `GdkScrollMask`
			* `GdkTouchMask`: no check
			* [x] `GdkSmoothScrollMask`
			* `GdkTouchpadGestureMsak`: no check
			* `GdkTablePadMask`: no check
