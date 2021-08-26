Event Structures memo
=====================

todo
----

* [x] add export list
* [x] add some to export list
* [x] separate `GdkEventType` to `Graphics.Gdk.EventStructures.GdkEventType`
* [x] remove `GdkEventType` from `Graphics.Gdk.EventStructures`
* [x] define `gdkModifierTypeCheck ::`
	`GdkModifierTypeSingleBit -> GdkModifierTypeMultiBits -> Bool`
* [ ] refactor export list
	+ [x] structure
		- [x] some
		- [x] add GdkEventType
			* [x] add type
			* [x] add patterns
	+ [ ] GDK EVENT
	+ [x] GDK EVENT ANY
	+ [ ] GDK EVENT KEY
		- [ ] use `GdkModifierTypeMultiBits` instesad of
			`[GdkModifierTypeSingleBit]`
	+ [ ] GDK EVENT BUTTON
	+ [ ] GDK EVENT SCROLL
	+ [ ] GDK EVENT MOTION
	+ [ ] GDK EVENT VISIBILITY
	+ [ ] GDK EVENT CROSSING
	+ [ ] GDK EVENT FOCUS
	+ [ ] GDK EVENT CONFIGURE
	+ [ ] GDK EVENT PROPERTY
	+ [ ] GDK EVENT WINDOW STYLE
		- [ ] Gdk Window State
* [ ] make `Graphics.Gdk.EventStructures.Internal`
* [ ] refactor export list
	+ [ ] structure
	+ [ ] others

old todo
--------

* [x] rename `GdkEvent` to `GdkEventTag`
* [x] rename `GdkEventSealed` to `GdkEvent`
* [x] define `gdkDisplayWithEventGet` instead of `gdkDisplayGetEvent`
* [x] define `gdkDisplayWithEventPeek` instead of `gdkDisplayPeekEvent`
* [x] remove `gdkDisplayPutEvent`
* [x] remove `mkGdkEvent`
* [ ] add `GdkEventGrabBroken`
	+ see `app/try-seat.hs`
* [ ] add rest
* [ ] add export list

Types
-----

### now

* [x] GdkEvent
* [x] GdkEventAny
* [x] GdkEventKey
* [x] GdkEventButton
* [x] GdkEventScroll
* [x] GdkEventMotion
	+ [x] define BoolInt16
	+ [x] others
* [x] GdkEventVisibility
	+ [x] `GdkEventVisibilityRaw`
	+ [x] `GdkEventGdkVisibilityNotify`
	+ [x] `GdkEventVisibility`
	+ [x] `gdkEventVisibility`
	+ [x] remove `tryGdkEventVisibilitySealedWindow`
* [x] GdkEventCrossing
	+ [x] add `GdkEnterNotifyMask` and `GdkLeaveNotifyMask`
	+ [x] `GdkCrossingMode`
	+ [x] `GdkNotifyType`
	+ [x] `GBoolean`
	+ [x] `GdkEventCrossingRaw`
		- [x] some
		- [x] `x_root`
		- [x] `y_root`
		- [x] `mode`
		- [x] `detail`
		- [x] `focus`
		- [x] `state`
	+ [x] `GdkEventGdkEnterNotify`
	+ [x] `GdkEventGdkLeaveNotify`
	+ [x] `GdkEventCrossing`
	+ [x] `gdkEventCrossing`
* [x] GdkEventFocus
	+ [x] `GdkEventFocusRaw`
	+ [x] `GdkEventGdkFocusChange`
	+ [x] `GdkEventFocus`
	+ [x] `gdkEventFocus`
* [x] GdkEventConfigure
	+ [x] `GdkEventConfigureRaw`
	+ [x] `GdkEventGdkConfigure`
	+ [x] `GdkEventConfigure`
	+ [x] `gdkEventConfigure`
	+ [x] remove `tryGdkEventSealedConfigureWindow`
* [x] GdkEventWindowState
	+ [x] try it
	+ [x] `newtype GdkWindowStates`
	+ [x] `newtype GdkWindowState`
	+ [x] struct `GdkEventWindowStateRaw`
	+ [x] `pattern GdkEventGdkWindowState`
	+ [x] `data GdkEventWindowState`
	+ [x] function `gdkEventWindowState`

### not now

* GdkEventTouch
* [x] GdkEventExpose
	+ remove it
* GdkEventSelection
* [x] GdkEventProperty
	+ remove it
* [x] `gdkPropertyGet` in `Graphics.Gdk.PropertiesAndAtoms.Properties`
	+ remove it and this module
* GdkEventDND
* GdkEventProximity
* GdkEventSetting
* GdkEventOwnerChange
* GdkEventGrabBroken
* GdkEventTouchpadSwipe
* GdkEventTouchpadPinch
* GdkEventPadButton
* GdkEventPadAxis
* GdkEventPadGroupMode

Values
------

* [x] GdkEventType
* [x] GdkScrollDirection
* [x] GdkVisibilityState
* [ ] GdkCrossingMode
* [ ] GdkNotifyType
* [ ] GdkPropertyState
* [ ] GdkWindowState
* [ ] GdkSettingAction
* [ ] GdkOwnerChange
