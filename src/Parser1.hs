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
import qualified Data.List as L
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe

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

months = [1..12] :: [Int]
years = [2019..] :: [Int]

newtype URL = URL String
newtype TagClassName = TagClassName String

-- tagNameToStr :: TagClassName -> String
-- tagNameToStr (TagClassName str) = str

urlToStr :: URL -> String
urlToStr (URL str) = str

mainUrl :: URL
mainUrl = URL "https://www.philharmonia.spb.ru"

makeUrl :: Int -> Int -> URL
makeUrl month year = URL $ (urlToStr mainUrl) ++ "/afisha/?ev_y=" ++ (show year) ++ "&ev_m=" ++ (show month)

oneItemTag :: TagClassName
oneItemTag = TagClassName "afisha_list_item zal"

dataTag :: TagClassName
dataTag = TagClassName "afisha_li_data"

dayTag :: TagClassName
dayTag = TagClassName "date_day"

timeTag :: TagClassName
timeTag = TagClassName "date_h"

imgTag :: TagClassName
imgTag = TagClassName "afisha_li_img"

descriptionTag :: TagClassName
descriptionTag = TagClassName "afisha_li_descr"

priceTag :: TagClassName
priceTag = TagClassName "afisha_li_function"


startContentTag :: TagClassName
startContentTag = TagClassName "afisha_list_items"

lastContentTag :: TagClassName
lastContentTag = TagClassName "bottom_sponsor"

titleTag :: TagClassName
titleTag = TagClassName "mer_item_title hand"


data Date = Date { day :: Int, month :: Int, year :: Int, time :: String} deriving (Eq, Show)
data Price = Price Int Int
data Concert = Concert {concertDate :: Date, concertPrice :: Maybe Price, concertInfo :: T.Text }

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

getTextFromTags :: [Tag LBS.ByteString] -> LBS.ByteString
getTextFromTags tags = delSpaces $ map fromTagText $ filter isTagText tags
            where
                delSpaces = LBS.unwords . LBS.words . LBS.unwords

getOneElemFromTags :: [Tag LBS.ByteString] -> TagClassName -> Int -> LBS.ByteString
getOneElemFromTags tags className idx =
                delSpaces $ fromTagText $ (!!idx) $ filter isTagText $
                    dropWhile (not . (isClassInTag className)) tags
            where
                delSpaces = LBS.unwords . LBS.words

getIntFromTag :: [Tag LBS.ByteString] -> TagClassName -> Int
getIntFromTag tags className = fst $ fromJust $ LBS.readInt $
                getOneElemFromTags tags className 0

parseDate :: [Tag LBS.ByteString] -> Date
parseDate block = Date {day = dayFromTag, month = 10, year = 2019, time = timeFromTag}
            where
                timeFromTag = LBS.unpack $ getOneElemFromTags block timeTag 0
                dayFromTag = getIntFromTag block dayTag


-- parseOneBlock :: [Tag LBS.ByteString] -> Concert
-- parseOneBlock block = Concert {date = dateFromBlock, }
--             where
--                 dateFromBlock = parseData $ fromTagToTag dataTag imgTag block

parseAfishaList :: LBS.ByteString -> [[Tag LBS.ByteString]]
parseAfishaList body = partitions (isClassInTag oneItemTag) $ mainContent tagList
            where
                tagList :: [Tag LBS.ByteString]
                tagList = parseTags body

parseInfo :: [Tag LBS.ByteString] -> URL
parseInfo block = urlInfo
            where
                urlInfo = URL $ (urlToStr mainUrl) ++ (fromAttrib "href" $ LBS.unpack <$> head (dropWhile (not . (isClassInTag titleTag)) block))



getHtml :: URL -> IO (LBS.ByteString)
getHtml (URL url) = do
              manager <- newManager tlsManagerSettings
              request <- parseRequest url
              response <- httpLbs request manager
              return $ responseBody response

getHtmlStr :: [URL] -> LBS.ByteString -> IO ()
getHtmlStr [] res = return res
getHtmlStr x:urls = do
              map getHtml urls

parseFun :: IO ()
parseFun = do
            body <- getHtml (makeUrl 10 2019)
            let items = parseAfishaList body
--             let dates = map (show . parseDate) items
            let infoUrls = map (parseInfo) items
            let strs = parseTags <$> getHtmlStr infoUrls
            print $ L.length strs
--             putStrLn $ L.intercalate "###\n##" info

--             map (LBS.unwords . LBS.words) (parseFromTag body)
--             LBS.putStrLn $ LBS.unlines $ prices
--             LBS.putStrLn $ LBS.unlines (fromTagToTag tag1 tag1 (parseTags body))
--             body2 <- getHtml (makeUrl 11 2019)
--             $ LBS.intercalate "#####"
--             LBS.putStrLn $ LBS.unlines (parseByTag oneItem body2)
--             LBS.putStrLn $ LBS.unlines (parseByTag oneItem body)

