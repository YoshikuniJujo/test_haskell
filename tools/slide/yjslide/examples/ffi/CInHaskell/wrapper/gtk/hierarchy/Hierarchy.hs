{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

module Hierarchy (
	SomeGObject(..),
	GObject(..),
	Pointer(..),

	castGObject,

	GtkObject(..),
	gtkObjectToGObject,
	gtkObjectFromGObject,

	GtkWidget(..),
	SomeGtkWidget(..),
	gtkWidgetToGObject,
	gtkWidgetFromGObject,

	GtkContainer(..),
	gtkContainerToGObject,
	gtkContainerFromGObject,

	GtkBin(..),
	gtkBinToGObject,
	gtkBinFromGObject,

	GtkButton(..),
	SomeGtkButton(..),
	gtkButtonToGObject,
	gtkButtonFromGObject,

	GtkWindow(..),
	SomeGtkWindow(..),
	gtkWindowToGObject,
	gtkWindowFromGObject,
) where

import Data.Typeable
import Foreign.Ptr

data SomeGObject = forall g . GObject g => SomeGObject g deriving Typeable

class (Typeable g, Pointer g) => GObject g where
	toGObject :: g -> SomeGObject
	fromGObject :: SomeGObject -> Maybe g

	toGObject = SomeGObject
	fromGObject (SomeGObject g) = cast g

class Pointer p where
	pointer :: p -> Ptr p
	fromPointer :: Ptr p -> p

data GtkObject = forall g . GObject g => GtkObject g deriving Typeable

instance Pointer GtkObject where
	pointer (GtkObject g) = castPtr $ pointer g

instance GObject GtkObject

gtkObjectToGObject :: GObject g => g -> SomeGObject
gtkObjectToGObject = toGObject . GtkObject

gtkObjectFromGObject :: GObject g => SomeGObject -> Maybe g
gtkObjectFromGObject g = do
	GtkObject gtk <- fromGObject g
	cast gtk

data GtkWidget = forall g . GObject g => GtkWidget g deriving Typeable
data SomeGtkWidget = SomeGtkWidget (Ptr SomeGtkWidget) deriving Typeable

instance Pointer GtkWidget where
	pointer (GtkWidget g) = castPtr $ pointer g
	fromPointer p = GtkWidget $ fromPointer $ (castPtr p :: Ptr SomeGtkWidget)

instance GObject GtkWidget where
	toGObject = gtkObjectToGObject
	fromGObject = gtkObjectFromGObject

gtkWidgetToGObject :: GObject g => g -> SomeGObject
gtkWidgetToGObject = toGObject . GtkWidget

gtkWidgetFromGObject :: GObject g => SomeGObject -> Maybe g
gtkWidgetFromGObject g = do
	GtkWidget w <- fromGObject g
	cast w

instance Pointer SomeGtkWidget where
	pointer (SomeGtkWidget p) = p
	fromPointer = SomeGtkWidget

instance GObject SomeGtkWidget where
	toGObject = gtkWidgetToGObject
	fromGObject = gtkWidgetFromGObject

data GtkContainer = forall g . GObject g => GtkContainer g deriving Typeable

instance Pointer GtkContainer where
	pointer (GtkContainer g) = castPtr $ pointer g

instance GObject GtkContainer where
	toGObject = gtkWidgetToGObject
	fromGObject = gtkWidgetFromGObject

gtkContainerToGObject :: GObject g => g -> SomeGObject
gtkContainerToGObject = toGObject . GtkContainer

gtkContainerFromGObject :: GObject g => SomeGObject -> Maybe g
gtkContainerFromGObject g = do
	GtkContainer c <- fromGObject g
	cast c

data GtkBin = forall g . GObject g => GtkBin g deriving Typeable

instance Pointer GtkBin where
	pointer (GtkBin g) = castPtr $ pointer g

instance GObject GtkBin where
	toGObject = gtkContainerToGObject
	fromGObject = gtkContainerFromGObject

gtkBinToGObject :: GObject g => g -> SomeGObject
gtkBinToGObject = toGObject . GtkBin

gtkBinFromGObject :: GObject g => SomeGObject -> Maybe g
gtkBinFromGObject g = do
	GtkBin b <- fromGObject g
	cast b

data GtkButton = forall g . GObject g => GtkButton g deriving Typeable

instance Pointer GtkButton where
	pointer (GtkButton g) = castPtr $ pointer g

instance GObject GtkButton where
	toGObject = gtkBinToGObject
	fromGObject = gtkBinFromGObject

gtkButtonToGObject :: GObject g => g -> SomeGObject
gtkButtonToGObject = toGObject . GtkButton

gtkButtonFromGObject :: GObject g => SomeGObject -> Maybe g
gtkButtonFromGObject g = do
	GtkButton b <- fromGObject g
	cast b

data SomeGtkButton = SomeGtkButton (Ptr SomeGtkButton) deriving Typeable

instance Pointer SomeGtkButton where
	pointer (SomeGtkButton p) = p

instance GObject SomeGtkButton where
	toGObject = gtkButtonToGObject
	fromGObject = gtkButtonFromGObject

data GtkWindow = forall g . GObject g => GtkWindow g deriving Typeable

instance Pointer GtkWindow where
	pointer (GtkWindow g) = castPtr $ pointer g

instance GObject GtkWindow where
	toGObject = gtkBinToGObject
	fromGObject = gtkBinFromGObject

gtkWindowToGObject :: GObject g => g -> SomeGObject
gtkWindowToGObject = toGObject . GtkWindow

gtkWindowFromGObject :: GObject g => SomeGObject -> Maybe g
gtkWindowFromGObject g = do
	GtkWindow w <- fromGObject g
	cast w

data SomeGtkWindow = SomeGtkWindow (Ptr SomeGtkWindow) deriving Typeable

instance Pointer SomeGtkWindow where
	pointer (SomeGtkWindow p) = p

instance GObject SomeGtkWindow where
	toGObject = gtkWindowToGObject
	fromGObject = gtkWindowFromGObject

castGObject :: (GObject g, GObject h) => g -> Maybe h
castGObject = fromGObject . toGObject
