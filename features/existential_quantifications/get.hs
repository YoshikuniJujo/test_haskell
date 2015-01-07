{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

import Data.Typeable

data Object = forall x . Typeable x => Object x

getObject :: Typeable x => Object -> Maybe x
getObject (Object x) = cast x
