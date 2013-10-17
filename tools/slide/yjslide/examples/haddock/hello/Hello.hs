-- | This is greeting module

module Hello (
	-- * documentation
	-- $doc
	-- ** type
	String,
	-- ** functions
	greeting
) where

-- $doc
-- Here is large chunk of documentation which may be refferred to
-- the name $doc
--
-- Good-bye!
--
-- The following characters have special meanings
-- \/, \', \`, \", \@, \<,
-- \>,
--
-- \*,
--
-- \-,
-- \>>>
-- &#x3BB;&#x3bb;&#955;\&#x3BB;
--
-- @
-- f x = x + x &#x3BB;
-- @
--
-- > g x = x * 42 &#x3BB;
--
-- This module defines the type 'String'.
--
-- This module defines the type 'Char'.
--
-- /emphasis/
-- @monospace@
-- @'greeting'@
--
-- "Main"
--
-- * first item
--
-- * second item
--
-- (8) first item
--
-- 999. second item
--
-- [@foo@] The description of @foo@
--
-- [@bar@] The description of @bar@
--
-- <http://google.com>
--
-- "Hello#here"

-- |
-- greeting [name] => \"Hello, [name]!\"
--
-- >>> greeting "world"
-- "Hello, world!"
--
-- >>> greeting "Yoshikuni Jujo"
-- "Hello, Yoshikuni Jujo!"
--
-- #here#

greeting :: String {- ^ name -} -> String {- ^ greeting -}
greeting name = "Hello, " ++ name ++ "!"

-- ^
-- greeting
--
-- >>> greeting "Manami Jujo"
-- "Hello, Manami Jujo!"
