module Main where

-- import Network.HTTP
-- import Text.HTML.TagSoup
--
-- openURL :: String -> IO String
-- openURL x = getResponseBody =<< simpleHTTP (getRequest x)
--
-- currentTime :: IO ()
-- currentTime = do
--     tags <- parseTags <$> openURL "http://rzd.ru"
--     let time = show (head (dropWhile (~/= "<a >") tags))
-- --     let time = length tags in
--     putStrLn time
--
-- main :: IO ()
-- main = currentTime


--
-- import Text.HTML.Scalpel
-- import qualified Network.HTTP.Client as HTTP
-- import qualified Network.HTTP.Client.TLS as HTTP
-- import qualified Network.HTTP.Types.Header as HTTP
--
--
-- -- Create a new manager settings based on the default TLS manager that updates
-- -- the request headers to include a custom user agent.
-- managerSettings :: HTTP.ManagerSettings
-- managerSettings = HTTP.tlsManagerSettings {
--   HTTP.managerModifyRequest = \req -> do
--     req' <- HTTP.managerModifyRequest HTTP.tlsManagerSettings req
--     return $ req' {
--       HTTP.requestHeaders = (HTTP.hUserAgent, "My Custom UA")
--                           : HTTP.requestHeaders req'
--     }
-- }
--
-- main = do
--     manager <- Just <$> HTTP.newManager managerSettings
--     html <- scrapeURLWithConfig (def { manager }) url $ htmls anySelector
--     maybe printError printHtml html
--   where
--     url = "https://www.google.com"
--     printError = putStrLn "Failed"
--     printHtml = mapM_ putStrLn

-- import Data.Maybe
-- import Data.Encoding
-- import Prelude
-- import Network.HTTP.Client
-- import Network.HTTP.Client.TLS   (tlsManagerSettings)
-- import Network.HTTP.Types.Status (statusCode)
-- import Text.HTML.TagSoup
-- import Text.Html.Encoding.Detection
-- import Text.Html.Encoding.Detection
-- import System.IO
-- -- import Data.Text.Encoding
-- import qualified Data.ByteString.Lazy.Char8 as DBS
-- import qualified Data.ByteString as DBS

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




-- url = "https://www.philharmonia.spb.ru/afisha/"
-- main :: IO ()
-- main = do
--     manager <- newManager tlsManagerSettings
--     request <- parseRequest url
--     response <- httpLbs request manager
--     let resHtml = responseBody response
-- --     let encd = detect resHtml
--     let resTags = parseTags resHtml
--     print resTags
--     let rDBS.putStrLnefr =  renderTags (filter (isTagText) resTags)
--     let resalt = map (DBS.putStrLn . fromTagText) refr
--     DBS.putStrLn refr


--     let refr = show (head (dropWhile (isTagText && (~\= "<a class=mer_item_title hand >")) resTags))
-- --     print enc
-- --     let decSt = fromJust enc
--     putStrLn refr

--     print $ show (detect resHtml)
--     putStrLn $ show (responseStatus response)
--     putStrLn $ "The status code was: " ++
--                show (statusCode $ responseStatus response)
--     print $ responseBody response
--     te <- mkTextEncoding "CP1251"
--     out <- openFile "/home/alex/projects/spec_haskell/concert-afisha/1.txt" WriteMode
--     hSetEncoding out te

import Parser1

main :: IO ()
main = do
    parseFun