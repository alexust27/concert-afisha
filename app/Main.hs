module Main where

--     print $ show (detect resHtml)
--     putStrLn $ show (responseStatus response)
--     putStrLn $ "The status code was: " ++
--                show (statusCode $ responseStatus response)
--     print $ responseBody response
--     te <- mkTextEncoding "CP1251"
--     out <- openFile "/home/alex/projects/spec_haskell/concert-afisha/1.txt" WriteMode
--     hSetEncoding out te


import Common (URL(..), Concert(..), Info(..), Person(..), printConcert)
import Parser1 (parseFun)
import Parser2 (parseFun2)
import Database

main :: IO ()
main = do
--   r <- doSome
--   print r
    let (m, y) = (11, 2019)
    concerts <- parseFun m y
    concerts2 <- parseFun2 m y
    mapM_ printConcert concerts
    mapM_ printConcert concerts2
