Event Structures memo
=====================

* [x] rename `GdkEvent` to `GdkEventTag`
* [x] rename `GdkEventSealed` to `GdkEvent`
* [x] define `gdkDisplayWithEventGet` instead of `gdkDisplayGetEvent`
* [x] define `gdkDisplayWithEventPeek` instead of `gdkDisplayPeekEvent`
* [x] remove `gdkDisplayPutEvent`
* [x] remove `mkGdkEvent`
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
* [ ] GdkEventConfigure
* [ ] GdkEventProperty
* [ ] GdkEventSelection
* [ ] GdkEventDND
* [ ] GdkEventProximity
* [ ] GdkEventWindowState
* [ ] GdkEventSetting
* [ ] GdkEventOwnerChange
* [ ] GdkEventGrabBroken
* [ ] GdkEventTouchpadSwipe
* [ ] GdkEventTouchpadPinch
* [ ] GdkEventPadButton
* [ ] GdkEventPadAxis
* [ ] GdkEventPadGroupMode

### not now

* GdkEventTouch
* [x] GdkEventExpose
	+ remove it

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
