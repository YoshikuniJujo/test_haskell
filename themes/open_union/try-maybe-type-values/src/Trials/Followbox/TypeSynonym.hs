{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.TypeSynonym (
	-- * FIELD
	WindowTitle, Position, LineWidth, FontName, FontSize,
	-- * GITHUB
	GithubUserName, GithubToken,
	-- * OTHERS
	Uri, Browser, ErrorMessage ) where

import Data.ByteString as BS

-- FIELD

type WindowTitle = String
type Position = (Integer, Integer)
type LineWidth = Integer
type FontName = String
type FontSize = Double

-- GITHUB

type GithubUserName = BS.ByteString
type GithubToken = BS.ByteString

-- OTHERS

type Uri = String
type Browser = FilePath
type ErrorMessage = String

