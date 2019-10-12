-- Общие функции для парсинга сайтов
module Common
( Concert(..)
, Date(..)
, Info(..)
, Person(..)
, Price(..)
, TagClassName(..)
, URL(..)
, isClassInTag
, fromTagToTag
, getHtml
, getIntFromTag
, getOneElemFromTags
, getTextFromTags
, printConcert
, strToPrice
, urlToStr
) where


import Network.HTTP.Client (httpLbs, newManager, parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
-- import Network.HTTP.Types.Status (statusCode)
-- import Text.Html.Encoding.Detection
import Text.HTML.TagSoup (Tag, Tag ( TagOpen ), isTagText, fromAttrib, fromTagText)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.ByteString.Lazy.Char8 as LBS


newtype URL = URL String deriving (Show, Eq)
newtype TagClassName = TagClassName String

data Date = Date { day    :: Int
                 , month  :: Int
                 , year   :: Int
                 , time   :: String
                 } deriving (Eq, Show)

data Price = Price ( [Int]
                   , URL
                   ) deriving (Eq, Show)

data Person = Person { name       :: LBS.ByteString
                     , role       :: LBS.ByteString
                     , personId   :: String
                     } deriving (Show, Eq)

data Info = Info { title      :: [LBS.ByteString]
                 , moreInfo   :: LBS.ByteString
                 , persons    :: [Person]
                 , ansambles  :: [LBS.ByteString]
                 , music      :: [LBS.ByteString]
                 }

data Concert = Concert { concertDate    :: Date
                       , concertInfo    :: Info
                       , concertPlace   :: LBS.ByteString
                       , concertPrice   :: Maybe Price
                       , urlAbout       :: URL
                       }

-- months = [1..12] :: [Int]
-- years = [2019..] :: [Int]


urlToStr :: URL -> String
urlToStr (URL str) = str

-- tagNameToStr :: TagClassName -> String
-- tagNameToStr (TagClassName str) = str

isClassInTag :: TagClassName -> Tag LBS.ByteString -> Bool
isClassInTag (TagClassName className) tag@(TagOpen _ _) = T.isInfixOf (T.pack className) $ T.pack
                                                        $ fromAttrib "class" $ LBS.unpack <$> tag
isClassInTag _ _ = False

-- возвращает список тегов с тега с классом "start" до тега с классом "end" не включая их
fromTagToTag :: TagClassName -> TagClassName -> [Tag LBS.ByteString] -> [Tag LBS.ByteString]
fromTagToTag start end tags = takeWhile (not . (isClassInTag end))
                            $ tail $ dropWhile (not . (isClassInTag start)) tags

getTextFromTags :: [Tag LBS.ByteString] -> LBS.ByteString
getTextFromTags tags = delSpaces $ map fromTagText $ filter isTagText tags
  where
    delSpaces = LBS.unwords . LBS.words . LBS.unwords

getOneElemFromTags :: [Tag LBS.ByteString] -> TagClassName -> Int -> LBS.ByteString
getOneElemFromTags tags className idx =
  delSpaces $ fromTagText $ (!!idx) $ filter isTagText $ dropWhile (not . (isClassInTag className)) tags
  where
    delSpaces = LBS.unwords . LBS.words

getIntFromTag :: [Tag LBS.ByteString] -> TagClassName -> Int
getIntFromTag tags className = case LBS.readInt $ getOneElemFromTags tags className 0 of
  Just (i, _) -> i
  Nothing -> (-999)

strToPrice :: LBS.ByteString -> [Int]
strToPrice str = if L.null resList then []
                 else [minimum resList, maximum resList]
  where
    f st = case LBS.readInt st of
      Just (i, _) -> i
      Nothing -> (-1)
    resList = filter (> 0) $ map f (LBS.words str)

getHtml :: URL -> IO (LBS.ByteString)
getHtml (URL url) = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest url
  response <- httpLbs request manager
  return $ responseBody response


printConcert :: Concert -> IO()
printConcert concert = do
  (LBS.putStrLn . LBS.unlines . title . concertInfo) concert
  (print . concertDate) concert
  case concertPrice concert of
    Nothing -> putStrLn "No tickects"
    Just pr -> print pr
  (LBS.putStrLn . concertPlace) concert
  let people = (persons . concertInfo) concert
  mapM_ (LBS.putStrLn . name) people
  mapM_ (print . personId) people

  (LBS.putStrLn . moreInfo . concertInfo) concert
  (print . urlAbout) concert
  print "-----------------------------------"


-- Берет все теги из блока с заданным тегом не включая этот тег
-- takeAllFromTag :: String -> [Tag LBS.ByteString] -> [Tag LBS.ByteString]
-- takeAllFromTag tag block = takeAllTags (idx + 1) 1 []
--   where
--     len = L.length block
--     idx = case L.findIndex (~== tag) block of
--       Just i -> i
--       Nothing -> -1
--     takeAllTags :: Int -> Int -> [Tag LBS.ByteString] -> [Tag LBS.ByteString]
--     takeAllTags _ 0 res = L.reverse res
--     takeAllTags indx balance res =
--       if idx >= len then res
--       else if isTagOpen curTag then takeAllTags (indx + 1) (balance + 1) (curTag:res)
--       else if isTagClose curTag then takeAllTags (indx + 1) (balance - 1) (curTag:res)
--       else takeAllTags (indx + 1) balance (curTag:res)
--       where
--         curTag = (block !! indx)



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
