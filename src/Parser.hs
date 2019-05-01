{-# LANGUAGE MultiParamTypeClasses #-}

module Parser (parseCmd) where

import           Command
import           Data.Bool.Unicode
import           Data.Eq.Unicode
import           Data.Ord.Unicode
import           Data.Void
import           Permissions
import           Prelude
import           Text.Megaparsec
import           Text.Megaparsec.Char

type ErrorMsg = ErrorFancy Void

instance ShowErrorComponent e => ShowErrorComponent (ErrorFancy e) where
  showErrorComponent (ErrorFail msg) = msg
  showErrorComponent (ErrorIndentation ord ref actual) =
    "incorrect indentation (got " <> show (unPos actual) <>
    ", should be " <> p <> show (unPos ref) <> ")"
    where
      p = case ord of
            LT -> "less than "
            EQ -> "equal to "
            GT -> "greater than "
  showErrorComponent (ErrorCustom a) = showErrorComponent a

parseAdduser ∷ Parsec ErrorMsg String Command
parseAdduser = do
  string "adduser"
  space1
  user ← some letterChar
  pure $ Adduser user

parseGroupadd ∷ Parsec ErrorMsg String Command
parseGroupadd = do
  string "groupadd"
  space1
  group ← some letterChar
  pure $ Groupadd group

parseUsermod ∷ Parsec ErrorMsg String Command
parseUsermod = do
  string "usermod -a -G"
  space1
  group ← some letterChar
  space1
  user ← some letterChar
  pure $ Usermod group user

parseSu ∷ Parsec ErrorMsg String Command
parseSu = do
  string "su"
  space1
  user ← some letterChar
  pure $ Su user

parseFilepath ∷ (MonadParsec e s m, Token s ~ Char) ⇒ m [Char]
parseFilepath = some printChar

parseTouch ∷ Parsec ErrorMsg String Command
parseTouch = do
  string "touch"
  space1
  fp ← parseFilepath
  pure $ Touch fp

parseRead ∷ Parsec ErrorMsg String Command
parseRead = do
  string "read"
  space1
  fp ← parseFilepath
  pure $ Read fp

parseExec ∷ Parsec ErrorMsg String Command
parseExec = do
  string "exec"
  space1
  fp ← parseFilepath
  pure $ Exec fp

parseWrite ∷ Parsec ErrorMsg String Command
parseWrite = do
  string "write"
  space1
  fp ← parseFilepath
  pure $ Write fp

parseChmod ∷ Parsec ErrorMsg String Command
parseChmod = do
  string "chmod"
  space1
  d1 ← (\x -> [x]) <$> oneOf ['0'..'7']
  d2 ← (\x -> [x]) <$> oneOf ['0'..'7']
  d3 ← (\x -> [x]) <$> oneOf ['0'..'7']
  space1
  fp ← parseFilepath
  let n1 = read d1 :: Int
  let n2 = read d2 :: Int
  let n3 = read d3 :: Int
  pure $ Chmod { owner = intToPermissions n1, group = intToPermissions n2, other = intToPermissions n3, file = fp }

parseChown :: Parsec ErrorMsg String Command
parseChown = do
  string "chown"
  space1
  user <- some letterChar
  space1
  group <- some letterChar
  space1
  fp <- some letterChar
  pure $ Chown user group fp

parseCurrentUser ∷ Parsec ErrorMsg String Command
parseCurrentUser = do
  string "currentuser"
  pure $ CurrentUser

parseCmd ∷ String → Maybe Command
parseCmd = parseMaybe $ parseAdduser
  <|> parseGroupadd
  <|> parseUsermod
  <|> parseSu
  <|> parseTouch
  <|> parseRead
  <|> parseExec
  <|> parseWrite
  <|> (try parseChmod)
  <|> parseChown
  <|> parseCurrentUser
