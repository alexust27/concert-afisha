module Database
( doSome
) where

import Database.PostgreSQL.Simple
import Control.Monad
import Control.Applicative

-- doSome :: IO Int.Int64
doSome = do
  conn <- connect defaultConnectInfo {
    connectDatabase = "haskell"
    c
  }
  putStrLn "2 + 2"
  mapM_ print =<< ( query_ conn "select 2 + 2" :: IO [Only Int] )

  putStrLn "3 + 5"
  mapM_ print =<< ( query conn "select ? + ?" (3 :: Int, 5 :: Int) :: IO [Only Int] )

  putStrLn "Enter a word"
  word <- getLine
  execute conn "insert into words (word) values (?)" $ Only word
