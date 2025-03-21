{-# LANGUAGE TupleSections, TypeFamilies, FlexibleContexts, RankNTypes,
	PackageImports #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Pipe (
	PipeClass(..), PipeChoice(..), (=@=), runPipe_, convert,
	Pipe, finally, bracket ) where

import Data.Pipe.Core
