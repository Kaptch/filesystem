{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Prelude

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UserEntry
  name String
  UniqueUser name
  deriving Show Eq Read
GroupEntry
  name String
  UniqueGroup name
  deriving Show Eq Read
UserToGroupEntry
  user UserEntryId
  group GroupEntryId
  deriving Show Eq Read
FileEntry
  name String
  owner UserEntryId
  group GroupEntryId
  ownerPermissions Int
  groupPermissions Int
  otherPermissions Int
  UniqueFile name
  deriving Show Eq Read
|]
