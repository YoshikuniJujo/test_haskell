{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds, DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Account where

import Database.Record.TH.SQLite3 (defineTable)

$(defineTable "db/examples.db" "account")
