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
* [ ] GdkEventExpose
* [ ] GdkEventVisibility
* [ ] GdkEventCrossing
* [ ] GdkEventFocus
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

Values
------

* [x] GdkEventType
* [x] GdkScrollDirection
* [ ] GdkVisibilityState
* [ ] GdkCrossingMode
* [ ] GdkNotifyType
* [ ] GdkPropertyState
* [ ] GdkWindowState
* [ ] GdkSettingAction
* [ ] GdkOwnerChange
