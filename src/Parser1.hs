{-# LANGUAGE OverloadedStrings #-}

module Parser1
  ( parseFun
  ) where


import Network.HTTP.Client
import Network.HTTP.Client.TLS   (tlsManagerSettings)
-- import Network.HTTP.Types.Status (statusCode)
import Text.HTML.TagSoup
-- import Text.Html.Encoding.Detection
-- import System.IO
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LBS
-- import qualified Data.ByteString as LBS

-- toCP1251 :: Text -> ByteString
-- toCP1251 = pack . unpack . map replace where
--   replace l = case (lookup l table) of
--       (Just x) -> xzal347 event_type_0 ta afisha_list_item_done
--       (Nothing) -> l
--
--   table = fromList $ zip rus cpCodes
--   cpCodes = map toEnum (168:184:[192 .. 255]) :: [Char]
--   rus =  ['Ё', 'ё', 'А', 'Б', 'В', 'Г', 'Д', 'Е', 'Ж', 'З', 'И', 'Й', 'К', 'Л', 'М',
--          'Н', 'О', 'П', 'Р', 'С', 'Т', 'У', 'Ф', 'Х', 'Ц', 'Ч', 'Ш', 'Щ', 'Ъ', 'Ы',
--          'Ь', 'Э', 'Ю', 'Я', 'а', 'б', 'в', 'г', 'д', 'е', 'ж', 'з', 'и', 'й', 'к',
--          'л', 'м', 'н', 'о', 'п', 'р', 'с', 'т', 'у', 'ф', 'х', 'ц', 'ч', 'ш', 'щ',

months = [1..12] :: [Int]
years = [2019..] :: [Int]

newtype URL = URL String
newtype TagClassName = TagClassName String

-- tagNameToStr :: TagClassName -> String
-- tagNameToStr (TagClassName str) = str

usrToStr :: URL -> String
usrToStr (URL str) = str

mainUrl :: URL
mainUrl = URL "https://www.philharmonia.spb.ru/afisha"

makeUrl :: Int -> Int -> URL
makeUrl month year = URL $ (usrToStr mainUrl) ++ "/?ev_y=" ++ (show year) ++ "&ev_m=" ++ (show month)

oneItemTag :: TagClassName
oneItemTag = TagClassName "afisha_list_item "

descriptionTag :: TagClassName
descriptionTag = TagClassName "afisha_li_descr"

priceTag :: TagClassName
priceTag = TagClassName "afisha_li_function"

dataTag :: TagClassName
dataTag = TagClassName "afisha_li_data"

startContentTag :: TagClassName
startContentTag = TagClassName "afisha_list_items"

lastContentTag :: TagClassName
lastContentTag = TagClassName "bottom_sponsor"


-- titleTag :: TagClassName
-- titleTag = TagClassName "<a class=\"mer_item_title hand\">"

getHtml :: URL -> IO (LBS.ByteString)
getHtml (URL url) = do
          manager <- newManager tlsManagerSettings
          request <- parseRequest url
          response <- httpLbs request manager
          return $ responseBody response

-- newtype Time = Time String
-- newtype Date = Date String
-- data Price = Price Int Int
-- data Concert = Concert {price :: Maybe Price, info :: Text, time :: Time, date :: Date}

-- isEmptyList::[[Tag LBS.ByteString]] -> Bool
-- isEmptyList x = x /= []
-- crazyTag :: Tag a
-- crazyTag = TagOpen "a" [("class","mer_item_title")]


isClassInTag :: TagClassName -> Tag LBS.ByteString -> Bool
isClassInTag (TagClassName className) tag@(TagOpen _ _) =
            T.isInfixOf (T.pack className) $ T.pack $ fromAttrib "class" $ LBS.unpack <$> tag
isClassInTag _ _ = False


fromTagToTag :: TagClassName -> TagClassName -> [Tag LBS.ByteString] -> [Tag LBS.ByteString]
fromTagToTag start end tags = takeWhile (not . (isClassInTag end)) $
                              tail $ dropWhile (not . (isClassInTag start)) tags

mainContent :: [Tag LBS.ByteString] -> [Tag LBS.ByteString]
mainContent = fromTagToTag startContentTag lastContentTag

parseAfishaList :: LBS.ByteString -> [[Tag LBS.ByteString]]
parseAfishaList body =
              partitions (isClassInTag oneItemTag) $  mainContent tagList

parseFun :: IO ()
parseFun = do
            body <- getHtml (makeUrl 10 2019)
            let prices = map (LBS.unwords . LBS.words) (parseFromTag oneItemTag body)
            LBS.putStrLn $ LBS.unlines $ prices
--             LBS.putStrLn $ LBS.unlines (fromTagToTag tag1 tag1 (parseTags body))
--             body2 <- getHtml (makeUrl 11 2019)
-- $ LBS.intercalate "#####"
--             LBS.putStrLn $ LBS.unlines (parseByTag oneItem body2)
--             LBS.putStrLn $ LBS.unlines (parseByTag oneItem body)






--     prices= "<div class=\"mer_item_prices\">"
--     let titles = fromTagText <$> (filter isTagText llTags)
--     let clearT = LBS.filter (\x -> (x /= '\n') || (x /= '\r')) <$> titles
--     isEmptyList::[[Tag LBS.ByteString]] -> Bool
--     let isEmptyList x = x /= []
--     let clearT = LBS.unwords <$> (LBS.words <$> titles)

--     let next = fromAttrib (dropWhile (~/= "<a class=\"mer_item_title hand\">") resTags)
--     print resTags

--     let refr =  renderTags (filter (~== "<a class = ") resTags)
--     let resalt = map (LBS.putStrLn . fromTagText) refr
--     LBS.putStrLn resTags


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
