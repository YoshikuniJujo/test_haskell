{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.Types
import GHC.TypeLits

type family Foo x where
	Foo 0 = Int
--	Foo n = [Foo (n - 1)]

type family List x where
	List () = Int
	List x = [x]

class Length lst where
	listLength :: lst -> List lst -> Int

instance Length () where
	listLength () = id

instance {-# Overlappable #-} Length x where
	listLength undefined = length
