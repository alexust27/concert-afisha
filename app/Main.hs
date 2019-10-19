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

main :: IO ()
main = do
  gui
--   dropTables
--   createTables
--   let (m, y) = (11, 2019)
--   concerts <- parseFun m y
--   mapM_ printConcert concerts
--   mapM_ addConcertToDB concerts


--   res <- isConcertInDB $ (concerts !! 1)
--   print res
--   concerts2 <- parseFun2 m y
--   mapM_ printConcert concerts2
--   mapM_ addConcertToDB concerts2
