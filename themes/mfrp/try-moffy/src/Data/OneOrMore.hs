{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses,
	FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.OneOrMore (
	-- * Type
	OneOrMore,
	-- * Property
	-- ** Basic Property
	Projectable, Insertable,
	-- ** Expandable and Collapsable
	Expandable, Collapsable,
	-- ** Mergeable
	Mergeable, Selectable(..),
	-- * Function
	-- ** Single Type
	pattern Singleton, unSingleton,
	-- ** Multiple Type
	project, (>-),
	-- ** Expand and Collapse
	expand, collapse,
	-- ** Merge
	merge, merge' ) where

import Data.OneOrMore.Internal
