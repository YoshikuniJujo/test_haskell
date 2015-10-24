module Morpheus (Pill(Red), select) where

data Pill = Blue | Red deriving Show

select :: Pill -> String
select Blue = "The story ends."
select Red = "You stay in Wonderland."
