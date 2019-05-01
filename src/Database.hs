module Database where

import           Control.Monad.Reader
import           Data.Bool.Unicode
import           Data.Eq.Unicode
import           Data.Function.Unicode
import           Data.Ord.Unicode
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.Types
import           Model
import           Permissions
import           Prelude

asSqlBackendReader ∷ ReaderT SqlBackend m a → ReaderT SqlBackend m a
asSqlBackendReader = id

runActionDB action = runSqlite "fs.db" . asSqlBackendReader $ action

createDB ∷ IO ()
createDB = runActionDB $ runMigration migrateAll

initDB ∷ IO ()
initDB = do
  insertGroupEntry $ GroupEntry "users"
  gRoot ← insertGroupEntry $ GroupEntry "root"
  uRoot ← insertUserEntry $ UserEntry "root"
  insertUserToGroupEntry $ UserToGroupEntry uRoot gRoot
  return ()

insertUserEntry ∷ UserEntry → IO (Key UserEntry)
insertUserEntry u = runActionDB $ insert u

getUserEntry ∷ String → IO (Maybe (Entity UserEntry))
getUserEntry n = runActionDB $ getBy $ UniqueUser n

insertGroupEntry ∷ GroupEntry → IO (Key GroupEntry)
insertGroupEntry g = runActionDB $ insert g

getGroupEntry ∷ String → IO (Maybe (Entity GroupEntry))
getGroupEntry n = runActionDB $ getBy $ UniqueGroup n

insertUserToGroupEntry ∷ UserToGroupEntry → IO (Key UserToGroupEntry)
insertUserToGroupEntry ug@(UserToGroupEntry u g) = runActionDB $ insert $ ug

getUserToGroupEntries ∷ String → IO [Key GroupEntry]
getUserToGroupEntries n = do
  u ← runActionDB $ getBy $ UniqueUser n
  case u of
    Nothing → return []
    Just a  → runActionDB $ (map $ userToGroupEntryGroup ∘ entityVal) <$> selectList [UserToGroupEntryUser ==. (entityKey a)] []

insertFileEntry ∷ FileEntry → IO (Key FileEntry)
insertFileEntry f = runActionDB $ insert f

getFileEntry ∷ String → IO (Maybe (Entity FileEntry))
getFileEntry s = runActionDB $ getBy $ UniqueFile s

key ∷ Entity a → Key a
key = entityKey

val ∷ Entity a → a
val = entityVal

updateFilePermissions ∷ Key FileEntry → Permissions → Permissions → Permissions → IO ()
updateFilePermissions fileKey owner group other = runActionDB $ update fileKey [FileEntryOwnerPermissions =. permissionsToInt owner, FileEntryGroupPermissions =. permissionsToInt group, FileEntryOtherPermissions =. permissionsToInt other]

updateFileOwner ∷ Key FileEntry → Entity UserEntry → Entity GroupEntry → IO ()
updateFileOwner fileKey owner group = runActionDB $ update fileKey [FileEntryOwner =. key owner, FileEntryGroup =. key group]
