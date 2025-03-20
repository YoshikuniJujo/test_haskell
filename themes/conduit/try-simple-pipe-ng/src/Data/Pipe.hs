{-# LANGUAGE TupleSections, TypeFamilies, FlexibleContexts, RankNTypes,
	PackageImports #-}

module Data.Pipe (
	PipeClass(..), PipeChoice(..), (=@=), runPipe_, convert,
	Pipe, finally, bracket ) where

import Data.Pipe.Core
