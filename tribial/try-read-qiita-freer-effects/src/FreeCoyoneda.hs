{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FreeCoyoneda (module Free, module Coyoneda, toFC) where

import Free
import Coyoneda

toFC :: t a -> Free (Coyoneda t) a
toFC = Join . (Pure <$>) . coyoneda
