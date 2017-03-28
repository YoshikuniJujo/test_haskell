{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes, TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

sr :: forall t . (Show t, Read t) => String -> String
sr = show . read @t
