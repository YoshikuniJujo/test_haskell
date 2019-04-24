{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Greeting where

import Database.Record.TH.SQLite3 (defineTable)

$(defineTable "test.sqlite3" "greeting")
