GDK API Reference
=================

GdkSeat
-------

### gdk\_seat\_grab

```
GdkGrabStatus
gdk_seat_grab (
	GdkSeat *seat,
	GdkWindow *window,
	GdkSeatCapabilities capabilities,
	gboolean owner_events,
	GdkCursor *cursor,
	const GdkEvent *event,
	GdkSeatGrabPrepareFunc prepare_func,
	gpointer prepare_func_data );
```

Grabs the seat so that all events corresponding to the given capabilities are
passed to this application until the seat is ungrabbed with gdk\_seat\_ungrab(),
or the window becomes hidden.
This overrides any previous grab on the seat by this client.

As a rule of thumb, if a grab is desired over GDK\_SEAT\_CAPABILITY\_POINTER,
all other "pointing" capabilities (eg. GDK\_SEAT\_CAPABILITY\_TOUCH) should be
grabbed too, so the user is able to interact with all of those while the grab holds,
you should thus use GDK\_SEAT\_CAPABILITY\_ALL\_POINTING most commonly.

Grabs are used for operations which need complete control over the events
corresponding to the given capabilities.
For example in GTK+ this is used for Drag and Drop operations, popup menus and
such.

Note that if the event mask of a GdkWindow has selected both button press and
button release events, or touch end, then a press event will cause an automatic
grab until the button is released, equivalent to a grab on the window with
owner\_events set to TRUE.
This is done because most applications expect to receive paired press and
release events.

If you set up anything at the time you take the grab that needs to be cleaned
up when the grab ends, you should handle the GdkEventGrabBroken events that
are emitted when the grab ends unvoluntarily.

#### Parameters

##### seat

a GdkSeat

##### window

the GdkWindow which will own the grab

##### capabilities

capabilities that will be grabbed

##### owner\_events

if FALSE then all device events are reported with respect to window and are
only reported if selected by event\_mask.
If TRUE then pointer events for this application are reported as normal, but
pointer events outside this application are reported with respect to window and
only if selected by event\_mask.
In either mode, unreported events are discarded.

##### cursor

the cursor to display while the grab is active.
If this is NULL then the normal cursors are used for window and its descendants,
and the cursor for window is used elsewhere.

##### event

the event that is triggering the grab, or NULL if none is available.

##### prepare\_func

function to prepare the window to be grabbed, it can be NULL if window is
visible before this call.

##### prepare\_func\_data

user data to pass to prepare\_func

#### Returns

GDK\_GRAB\_SUCCESS if the grab was successful.
