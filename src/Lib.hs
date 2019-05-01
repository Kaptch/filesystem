module Lib where

import           Command
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bool.Unicode
import           Data.Eq.Unicode
import           Data.Function.Unicode
import           Data.Ord.Unicode
import           Database
import           Model
import           Parser
import           Permissions
import           Prelude

type AppState = String

type App =  StateT AppState (ExceptT IOException IO)

canRead ∷ FileEntry → App Bool
canRead fe = do
  u ← get
  uK ← liftIO $ getUserEntry u
  groupList ← liftIO $ getUserToGroupEntries u
  case (uK, groupList) of
    (Just _userKey, (groupKey:_)) → do
      if (uuu ≡ owner ∧ ownerP ∨ groupKey ≡ group ∧ groupP ∨ otherP ∨ u ≡ "root") then return True else return False
        where
          uuu = key _userKey
          owner = fileEntryOwner fe
          group = fileEntryGroup fe
          ownerP = readable $ intToPermissions $ fileEntryOwnerPermissions fe
          groupP = readable $ intToPermissions $ fileEntryGroupPermissions fe
          otherP = readable $ intToPermissions $ fileEntryOtherPermissions fe
    _ → return False

canExecute ∷ FileEntry → App Bool
canExecute fe = do
  u ← get
  uK ← liftIO $ getUserEntry u
  groupList ← liftIO $ getUserToGroupEntries u
  case (uK, groupList) of
    (Just _userKey, (groupKey:_)) → do
      if (uuu ≡ owner ∧ ownerP ∨ groupKey ≡ group ∧ groupP ∨ otherP ∨ u ≡ "root") then return True else return False
        where
          uuu = key _userKey
          owner = fileEntryOwner fe
          group = fileEntryGroup fe
          ownerP = executable $ intToPermissions $ fileEntryOwnerPermissions fe
          groupP = executable $ intToPermissions $ fileEntryGroupPermissions fe
          otherP = executable $ intToPermissions $ fileEntryOtherPermissions fe
    _ → return False

canWrite ∷ FileEntry → App Bool
canWrite fe = do
  u ← get
  uK ← liftIO $ getUserEntry u
  groupList ← liftIO $ getUserToGroupEntries u
  case (uK, groupList) of
    (Just _userKey, (groupKey:_)) → do
      if (uuu ≡ owner ∧ ownerP ∨ groupKey ≡ group ∧ groupP ∨ otherP ∨ u ≡ "root") then return True else return False
        where
          uuu = key _userKey
          owner = fileEntryOwner fe
          group = fileEntryGroup fe
          ownerP = writable $ intToPermissions $ fileEntryOwnerPermissions fe
          groupP = writable $ intToPermissions $ fileEntryGroupPermissions fe
          otherP = writable $ intToPermissions $ fileEntryOtherPermissions fe
    _ → return False

canCh ∷ FileEntry → App Bool
canCh fe = do
  u ← get
  uK ← liftIO $ getUserEntry u
  case uK of
    Just _userKey → do
      if uuu ≡ owner ∨ u ≡ "root" then return True else return False
        where
          uuu = key _userKey
          owner = fileEntryOwner fe
    _ → return False

eval ∷ String → App ()
eval cmd = do
  let p = parseCmd cmd
  case p of
    Nothing → liftIO $ putStrLn "Couldn't parse"
    Just a  → perform a

perform ∷ Command → App ()
perform (Adduser str) = do
  g ← liftIO $ getGroupEntry "users"
  case g of
    Just g1 → do
      uk ← liftIO $ insertUserEntry $ UserEntry str
      liftIO $ insertUserToGroupEntry $ UserToGroupEntry uk (key g1)
      return ()
    Nothing → return ()
perform (Groupadd str) = (liftIO $ insertGroupEntry $ GroupEntry str) >> return ()
perform (Usermod g u) = do
  u1 ← liftIO $ getUserEntry u
  g1 ← liftIO $ getGroupEntry g
  case (u1, g1) of
    (Just u2, Just g2) → (liftIO $ insertUserToGroupEntry $ UserToGroupEntry (key u2) (key g2)) >> return ()
    (Just _, _) → liftIO $ putStrLn "There is no such group"
    (_, Just _) → liftIO $ putStrLn "There is no such user"
    (_, _) → liftIO $ putStrLn "There aren't such user and group"
perform (Su str) = do
  u ← liftIO $ getUserEntry str
  case u of
    (Just _) → put str >> return ()
    _        → liftIO $ putStrLn "There is no such user"
perform (Touch fp) = do
  u ← get
  u1 ← liftIO $ getUserEntry u
  case u1 of
    Just u2 → do
      l ← liftIO $ getUserToGroupEntries $ userEntryName (val u2)
      case l of
        (g:xs) → (liftIO $ insertFileEntry $ FileEntry fp (key u2) g 7 5 5) >> return ()
        _      → liftIO $ putStrLn "Couldn't get primary user's group"
    Nothing → liftIO $ putStrLn (u ++ " doesn't exist")
perform (Read fp) = do
  fe ← liftIO $ getFileEntry fp
  case fe of
    Just fe1 → do
      check ← canRead (val fe1)
      if check then liftIO $ putStrLn "Permission confirmed" else
        liftIO $ putStrLn "Permission denied"
    Nothing → liftIO $ putStrLn "File doesn't exist"
perform (Exec fp) = do
  fe ← liftIO $ getFileEntry fp
  case fe of
    Just fe1 → do
      check ← canExecute (val fe1)
      if check then liftIO $ putStrLn "Permission confirmed" else
        liftIO $ putStrLn "Permission denied"
    Nothing → liftIO $ putStrLn "File doesn't exist"
perform (Write fp) = do
  fe ← liftIO $ getFileEntry fp
  case fe of
    Just fe1 → do
      check ← canWrite (val fe1)
      if check then liftIO $ putStrLn "Permission confirmed" else
        liftIO $ putStrLn "Permission denied"
    Nothing → liftIO $ putStrLn "File doesn't exist"
perform (Chmod owner group other file) = do
  fe ← liftIO $ getFileEntry file
  case fe of
    Just fe1 → do
      check ← canCh (val fe1)
      if check then do
        liftIO $ updateFilePermissions (key fe1) owner group other
        liftIO $ putStrLn "Permission confirmed"
      else liftIO $ putStrLn "Permission denied"
    Nothing → liftIO $ putStrLn "File owner wasn't found"
perform (Chown u g fp) = do
  fe ← liftIO $ getFileEntry fp
  u1 ← liftIO $ getUserEntry u
  g1 ← liftIO $ getGroupEntry g
  case (fe, u1, g1) of
    (Just fe1, Just u2, Just g2) → do
      check ← canCh (val fe1)
      if check then do
        liftIO $ updateFileOwner (key fe1) u2 g2
        liftIO $ putStrLn "Permission confirmed"
      else liftIO $ putStrLn "Permission denied"
    _ → liftIO $ putStrLn "DB desync"
perform (CurrentUser) = do
  a ← get
  liftIO $ putStrLn a

loop ∷ App ()
loop = do
  cmd  ← liftIO getLine
  app  ← catchError (eval cmd) (\e → liftIO $ putStrLn $ show (e ∷ IOException)) >> return ()
  loop
