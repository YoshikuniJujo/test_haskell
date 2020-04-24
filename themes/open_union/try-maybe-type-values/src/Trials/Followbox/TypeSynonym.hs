{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.TypeSynonym (
	-- * FIELD
	WindowTitle, Position, LineWidth, FontName, FontSize,
	-- * GITHUB
	GithubUserName, GithubToken,
	-- * OTHERS
	Uri, Browser, ErrorMessage
	) where

import Data.ByteString as BS

type Position = (Integer, Integer)
type FontName = String
type FontSize = Double
type LineWidth = Integer

type Uri = String

type ErrorMessage = String

type Browser = FilePath

type WindowTitle = String

type GithubUserName = BS.ByteString
type GithubToken = BS.ByteString
