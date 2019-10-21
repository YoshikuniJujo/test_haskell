module Given where

import Expression

newtype Given i v = Given [Expression i v] deriving Show

-- removeVar :: Given i v -> v -> Given i v
-- removeVar

-- remVar :: [Expression i v] -> [Expression i v] 
