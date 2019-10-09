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

-- dataTag :: TagClassName
-- dataTag = TagClassName "afisha_li_data"

dayTag :: TagClassName
dayTag = TagClassName "date_day"

timeTag :: TagClassName
timeTag = TagClassName "date_h"

-- imgTag :: TagClassName
-- imgTag = TagClassName "afisha_li_img"

-- descriptionTag :: TagClassName
-- descriptionTag = TagClassName "afisha_li_descr"

-- priceTag :: TagClassName
-- priceTag = TagClassName "afisha_li_function"


startContentTag :: TagClassName
startContentTag = TagClassName "afisha_list_items"

lastContentTag :: TagClassName
lastContentTag = TagClassName "bottom_sponsor"

data Date = Date { day :: Int, month :: Int, year :: Int, time :: String} deriving (Eq, Show)
data Price = Price Int Int
-- data Music = Music {}
data Person = Person { name :: LBS.ByteString, role :: LBS.ByteString, personId :: Int } deriving (Show)
data Info = Info { title :: LBS.ByteString, moreInfo :: LBS.ByteString, persons :: [Person],
                   ansambles :: [LBS.ByteString], music::[LBS.ByteString] }
data Concert = Concert {concertDate :: Date, concertPrice :: Maybe Price, concertInfo :: Info }

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

-- TODO: fix month and year
parseDate :: [Tag LBS.ByteString] -> Date
parseDate block = Date {day = dayFromTag, month = 10, year = 2019, time = timeFromTag}
            where
                timeFromTag = LBS.unpack $ getOneElemFromTags block timeTag 0
                dayFromTag = getIntFromTag block dayTag

parseInfoTags :: [Tag LBS.ByteString] -> Info
parseInfoTags tags =
             let block = fromTagToTag (TagClassName "td right") (TagClassName "afisha_element_slider_next") tags
                 elementAge = TagClassName "afisha_element_age"
                 titleTag = TagClassName "afisha_element_title"
                 elementInfo = TagClassName "afisha_element_dett"
                 personsTag = TagClassName "ae_persons_main"
                 ansamblesTag = TagClassName "ansambles"
                 ansambleTag = TagClassName "ansamble_title"
                 musicTag = TagClassName "td ae_music"
                 in let
                        mainTitle = getTextFromTags titleBlock
                            where titleBlock = fromTagToTag titleTag elementAge block
                        mInfo = if L.any (isClassInTag elementInfo) block
                            then getTextFromTags infoBlock
                            else LBS.empty
                           where infoBlock = fromTagToTag elementInfo personsTag block
                        peoples = if L.any (isClassInTag personsTag) block
                            then map parsePeople $ partitions (isClassInTag (TagClassName "ae_persons_maini ta")) $
                                fromTagToTag personsTag ansamblesTag block
                            else []
                        curAnsambles = if L.any (isClassInTag ansambleTag) ansamblesBlock
                            then map getTextFromTags $ take 3 $ partitions (isClassInTag ansambleTag) ansamblesBlock
                            else []
                           where  ansamblesBlock = fromTagToTag ansamblesTag musicTag block
                        curMusic = map getTextFromTags $
                                partitions (isClassInTag (TagClassName "ae_music_auithor_o")) musicBlock
                           where musicBlock = dropWhile (not . (isClassInTag musicTag)) block
                    in
                     Info {title = mainTitle,
                           moreInfo = mInfo,
                           persons = peoples,
                           ansambles = curAnsambles,
                           music = curMusic}


parsePeople :: [Tag LBS.ByteString] -> Person
parsePeople tags = Person {name = personName, role = personRole }
            where
                personName = getTextFromTags $ fromTagToTag (TagClassName "nam") (TagClassName "rol") tags
                personRole = getTextFromTags $ dropWhile (not . (isClassInTag (TagClassName "rol"))) tags

-- parseOneBlock :: [Tag LBS.ByteString] -> Concert
-- parseOneBlock block = Concert {date = dateFromBlock, }
--             where
--                 dateFromBlock = parseData $ fromTagToTag dataTag imgTag block

parseAfishaList :: LBS.ByteString -> [[Tag LBS.ByteString]]
parseAfishaList body = partitions (isClassInTag oneItemTag) $ mainContent tagList
            where
                oneItemTag :: TagClassName
                oneItemTag = TagClassName "afisha_list_item zal"
                tagList :: [Tag LBS.ByteString]
                tagList = parseTags body
                mainContent :: [Tag LBS.ByteString] -> [Tag LBS.ByteString]
                mainContent = fromTagToTag startContentTag lastContentTag


getInfoUrl :: [Tag LBS.ByteString] -> URL
getInfoUrl block = URL $ (urlToStr mainUrl) ++
                         (fromAttrib "href" $ LBS.unpack <$> head (dropWhile (not . (isClassInTag titleTag)) block))
            where
                titleTag = TagClassName "mer_item_title hand"

getHtml :: URL -> IO (LBS.ByteString)
getHtml (URL url) = do
              manager <- newManager tlsManagerSettings
              request <- parseRequest url
              response <- httpLbs request manager
              return $ responseBody response

-- getHtmlSp :: URL -> Manager -> IO ()
-- getHtmlSp (URL url) manager = do
--          request <- parseRequest url
--          withResponse request manager $ \response -> do
--                 putStrLn $ "The status code was: " ++
--                            show (statusCode $ responseStatus response)
--                 let loop = do
--                                 bs <- brRead $ responseBody response
--                                 if S.null bs
--                                     then putStrLn "\nFinished response body"
--                                     else do
--                                         S.hPut stdout bs
--                                         loop
--                 loop

getHtmlStr :: [URL] -> [LBS.ByteString] -> IO ([LBS.ByteString])
getHtmlStr [] res = return res
getHtmlStr (x:urls) res = do
                  res2 <- getHtml x
                  print $ L.length urls
                  getHtmlStr urls (res2:res)

--                  res2 <- getHtml x
--                  print $ L.length urls
--                  getHtmlStr urls (res2:res)

-- fromIo :: [IO (LBS.ByteString)] -> IO ([Info])
-- fromIo [] = return []
-- fromIo (x:xs) = do
--                 x' <- x
--                 let tags = parseTags x'
--                 let res = parseInfoTags tags
--                 res2 <- fromIo xs
--                 return $ (res:res2)
-- testUrl = URL "https://www.philharmonia.spb.ru/afisha/323968/"
testUrl = URL "https://www.philharmonia.spb.ru/afisha/329945/"

parseFun :: IO ()
parseFun = do
--             manager <- newManager tlsManagerSettings
            body <- getHtml (makeUrl 10 2019)
--             body <- getHtml testUrl
            let blocks = parseAfishaList body
            let dates = map (show . parseDate) blocks
--             let infoUrls = map getInfoUrl blocks
--             infos <- sequence $ map getHtml infoUrls
--             print $ L.length $ map (parseInfoTags . parseTags) infos
--             print $ parseInfoTags $ parseTags $ head infos
--             let infos = parseInfoTags $ parseTags body
--             LBS.putStrLn $ title infos
--             LBS.putStrLn $ moreInfo infos
--             LBS.putStrLn $ LBS.unlines $ music infos
--             LBS.putStrLn $ LBS.unlines $ ansambles infos
--             putStrLn $ L.intercalate "###\n##" info

--             map (LBS.unwords . LBS.words) (parseFromTag body)
--             LBS.putStrLn $ LBS.unwords . LBS.words $ oneInfo
--             LBS.putStrLn $ LBS.unlines (fromTagToTag tag1 tag1 (parseTags body))
--             body2 <- getHtml (makeUrl 11 2019)
--             $ LBS.intercalate "#####"
--             LBS.putStrLn $ LBS.unlines (parseByTag oneItem body2)
--             LBS.putStrLn $ LBS.unlines (parseByTag oneItem body)

