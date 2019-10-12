module Main where

-- import Data.Encoding
-- import Text.Html.Encoding.Detection
-- import System.IO
-- -- import Data.Text.Encoding

-- toCP1251 :: Text -> ByteString
-- toCP1251 = pack . unpack . map replace where
--   replace l = case (lookup l table) of
--       (Just x) -> x
--       (Nothing) -> l
--
--   table = fromList $ zip rus cpCodes
--   cpCodes = map toEnum (168:184:[192 .. 255]) :: [Char]
--   rus =  ['Ё', 'ё', 'А', 'Б', 'В', 'Г', 'Д', 'Е', 'Ж', 'З', 'И', 'Й', 'К', 'Л', 'М',
--          'Н', 'О', 'П', 'Р', 'С', 'Т', 'У', 'Ф', 'Х', 'Ц', 'Ч', 'Ш', 'Щ', 'Ъ', 'Ы',
--          'Ь', 'Э', 'Ю', 'Я', 'а', 'б', 'в', 'г', 'д', 'е', 'ж', 'з', 'и', 'й', 'к',
--          'л', 'м', 'н', 'о', 'п', 'р', 'с', 'т', 'у', 'ф', 'х', 'ц', 'ч', 'ш', 'щ',
--          'ъ', 'ы', 'ь', 'э', 'ю', 'я']  :: [Char]



--     print $ show (detect resHtml)
--     putStrLn $ show (responseStatus response)
--     putStrLn $ "The status code was: " ++
--                show (statusCode $ responseStatus response)
--     print $ responseBody response
--     te <- mkTextEncoding "CP1251"
--     out <- openFile "/home/alex/projects/spec_haskell/concert-afisha/1.txt" WriteMode
--     hSetEncoding out te


-- import qualified Data.ByteString as LBS

-- toCP1251 :: Text -> ByteString
-- toCP1251 = pack . unpack . map replace where
--   replace l = case (lookup l table) of
--       (Just x) -> x
--       (Nothing) -> l
--
--   table = fromList $ zip rus cpCodes
--   cpCodes = map toEnum (168:184:[192 .. 255]) :: [Char]
--   rus =  ['Ё', 'ё', 'А', 'Б', 'В', 'Г', 'Д', 'Е', 'Ж', 'З', 'И', 'Й', 'К', 'Л', 'М',
--          'Н', 'О', 'П', 'Р', 'С', 'Т', 'У', 'Ф', 'Х', 'Ц', 'Ч', 'Ш', 'Щ', 'Ъ', 'Ы',
--          'Ь', 'Э', 'Ю', 'Я', 'а', 'б', 'в', 'г', 'д', 'е', 'ж', 'з', 'и', 'й', 'к',
--          'л', 'м', 'н', 'о', 'п', 'р', 'с', 'т', 'у', 'ф', 'х', 'ц', 'ч', 'ш', 'щ',


import qualified Data.ByteString.Lazy.Char8 as LBS
import Common (URL(..), Concert(..), Info(..), Person(..), printConcert)
import Parser1 (parseFun)
import Parser2 (parseFun2)


main :: IO ()
main = do
    let (m, y) = (11, 2019)
    concerts <- parseFun m y
--     concerts2 <- parseFun2 m y
    mapM_ printConcert concerts
--     mapM_ printConcert concerts2
