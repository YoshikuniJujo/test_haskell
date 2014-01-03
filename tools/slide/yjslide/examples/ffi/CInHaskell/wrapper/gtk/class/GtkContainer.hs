{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module GtkContainer (
	GtkContainer(..)
) where

import Foreign.Ptr
import GtkWidget

data GtkContainerPtr
class GtkContainer c where
	gtkContainerPtr :: c -> Ptr GtkContainerPtr
instance GtkContainer c => GtkWidget c where
	gtkWidgetPtr = castPtr . gtkContainerPtr
