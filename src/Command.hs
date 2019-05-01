module Command where

import           Permissions
import           Prelude

type User = String

type Group = String

data Command = Adduser User
  | Groupadd Group
  | Usermod Group User
  | Su User
  | Touch FilePath
  | Read FilePath
  | Exec FilePath
  | Write FilePath
  | Chmod
    {
      owner ∷ Permissions,
      group ∷ Permissions,
      other ∷ Permissions,
      file  ∷ FilePath
    }
  | Chown User Group FilePath
  | CurrentUser
    deriving (Show)
