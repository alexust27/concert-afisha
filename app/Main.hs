module Main where

--     print $ show (detect resHtml)
--     putStrLn $ show (responseStatus response)
--     putStrLn $ "The status code was: " ++
--                show (statusCode $ responseStatus response)
--     print $ responseBody response
--     te <- mkTextEncoding "CP1251"
--     out <- openFile "/home/alex/projects/spec_haskell/concert-afisha/1.txt" WriteMode
--     hSetEncoding out te


import Common (printConcert)
import Parser1 (parseFun)
import Parser2 (parseFun2)
import Database (addConcertToDB, createTables, dropTables)
import UI


updateByDate :: Int -> Int -> Int -> IO ()
updateByDate d m y = do
  dropTables
  createTables
  concerts <- parseFun d m y
  concerts2 <- parseFun2 d m y

--   mapM_ printConcert concerts
--   mapM_ printConcert concerts2
  mapM_ addConcertToDB concerts
  mapM_ addConcertToDB concerts2


main :: IO ()
main = do
  gui
--   let (d, m, y) = (21, 10, 2019)
--   updateByDate d m y
