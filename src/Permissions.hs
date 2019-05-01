module Permissions where

import           Data.Bool.Unicode
import           Data.Eq.Unicode
import           Data.Function.Unicode
import           Data.Ord.Unicode
import           Prelude

data Permissions = Permissions
  {
    readable, writable, executable ∷ Bool
  }
  deriving (Show)

intToPermissions ∷ Int → Permissions
intToPermissions i = Permissions { readable = a, writable = b, executable = c }
  where
    a = i - 4 ≥ 0
    b = (if a then i - 6 else i - 2) ≥ 0
    c = i `mod` 2 ≡ 1

permissionsToInt ∷ Permissions → Int
permissionsToInt (Permissions r w e) = a + b + c
  where
    a = if r then 4 else 0
    b = if w then 2 else 0
    c = if e then 1 else 0
