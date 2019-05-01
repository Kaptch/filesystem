module Main where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.State
import           Database             (createDB, initDB)
import           Lib
import           Prelude

main :: IO ()
main = do
  createDB
  catch initDB (\e → putStrLn $ show (e ∷ SomeException))
  let appState = "root"
  runExceptT $ evalStateT loop appState
  return ()
