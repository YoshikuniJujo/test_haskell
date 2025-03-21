{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Pipe (

	-- * DATA PIPE

	Pipe,

	-- * PIPE CLASSES

	PipeClass(..), PipeChoice(..),

	-- * TOOLS

	runPipe_, (=@=), convert, bracket, finally

	) where

import Data.Pipe.Core
