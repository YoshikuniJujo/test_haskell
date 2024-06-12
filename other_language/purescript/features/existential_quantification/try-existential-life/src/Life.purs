module Life where

import Data.Maybe
import LifeTypeable

data SomeLife = forall l . Life l => SomeLife l
