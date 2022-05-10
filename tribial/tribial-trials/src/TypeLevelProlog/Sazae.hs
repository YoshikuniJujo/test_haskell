{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TypeLevelProlog.Sazae where

data Namihei
data Fune
data Sazae
data Katsuo
data Wakame
data Masuo
data Tara

class Male x where male :: (); male = ()
class Female x where female :: (); female = ()
class Child c p where child :: (); child = ()

instance Male Namihei
instance Female Fune
instance Female Sazae
instance Male Katsuo
instance Female Wakame
instance Male Masuo
instance Male Tara

instance Child Sazae Namihei
instance Child Sazae Fune
instance Child Katsuo Namihei
instance Child Katsuo Fune
instance Child Wakame Namihei
instance Child Wakame Fune
instance Child Tara Masuo
instance Child Tara Sazae

class Father f c where father :: (); father = ()
instance (Child c f, Male f) => Father f c

class Mother m c where mother :: (); mother = ()
instance (Child c m, Female m) => Mother m c

class Grandfather gf x where grandfather :: (); grandfather = ()
instance (Father gf p, Child x p) => Grandfather gf x where grandfather = ()
