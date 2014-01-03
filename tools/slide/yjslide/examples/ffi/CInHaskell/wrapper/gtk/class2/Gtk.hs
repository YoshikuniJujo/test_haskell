module Gtk (
) where

import Foreign.Ptr

data SomeGObject = SomeGObject (Ptr SomeGObject) deriving Show
class GObject o where
	toGObject :: o -> SomeGObject
instance GObject SomeGObject where
	toGObject = id

fun1 :: GObject o => o -> String
fun1 = show . toGObject

data SomeGtkObject = SomeGtkObject (Ptr SomeGtkObject) deriving Show
class GtkObject o where
	toGtkObject :: o -> SomeGtkObject
instance GtkObject SomeGtkObject where
	toGtkObject = id
instance GObject SomeGtkObject where
	toGObject (SomeGtkObject p) = SomeGObject $ castPtr p

data SomeGtkBin = SomeGtkBin (Ptr SomeGtkBin) deriving Show
class GtkBin b where
	toGtkBin :: b -> SomeGtkBin
instance GtkBin SomeGtkBin where
	toGtkBin = id
instance GtkObject SomeGtkBin where
	toGtkObject (SomeGtkBin p) = SomeGtkObject $ castPtr p

data SomeGtkWindow = SomeGtkWindow (Ptr SomeGtkWindow) deriving Show
class GtkWindow w where
	toGtkWindow :: w -> SomeGtkWindow
instance GtkWindow SomeGtkWindow where
	toGtkWindow = id
instance GtkBin SomeGtkWindow where
	toGtkBin (SomeGtkWindow p) = SomeGtkBin $ castPtr p
