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
, getFirstTextFromTag
, getTextFromTags
, getTimeAndDate
, printConcert
, strToPrice
, urlToStr
) where


import Network.HTTP.Client (httpLbs, newManager, parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Text.HTML.TagSoup (Tag, Tag ( TagOpen ), isTagText, fromAttrib, fromTagText)
import Data.Char(isDigit)
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

data Person = Person { personName  :: String
                     , personRole  :: Maybe String
                     , personId    :: String
                     , isLiked     :: Bool
                     } deriving (Show, Eq)

data Info = Info { title           :: String
                 , addInfo         :: String
                 , moreInfo        :: String
                 , persons         :: [Person]
                 } deriving (Show)

data Concert = Concert { concertDate    :: Date
                       , concertInfo    :: Info
                       , concertPlace   :: String
                       , concertPrice   :: Maybe Price
                       , urlAbout       :: URL
                       } deriving (Show)

-- months = [1..12] :: [Int]
-- years = [2019..] :: [Int]

getTimeAndDate :: Date -> (String, String)
getTimeAndDate d = (time d, dat)
  where
    dat = show (day d) ++ "-" ++ show (month d) ++ "-" ++ show (year d)

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

getTextFromTags :: (LBS.ByteString -> String) -> [Tag LBS.ByteString] -> String
getTextFromTags toStrFun tags = delSpaces $ map (toStrFun . fromTagText) $ filter isTagText tags
  where
    delSpaces = L.unwords . L.words . L.unwords


getFirstTextFromTag :: (LBS.ByteString -> String) -> [Tag LBS.ByteString] -> TagClassName -> String
getFirstTextFromTag toStrF block className =  if L.null res then "" else toStrF $ fromTagText $ head res
  where
    res = dropWhile (not . isTagText) $ dropWhile (not . (isClassInTag className)) block


strToPrice :: String -> [Int]
strToPrice str = [mnPrice, max mxPrice mnPrice]
  where
    clearStr = L.unwords . L.words $ str
    mnPrice = toIntPr $ takeWhile (isDigit) clearStr
    mxPrice = toIntPr $ takeWhile (isDigit) $ dropWhile (not . isDigit) $ dropWhile (isDigit) clearStr
    toIntPr :: String -> Int
    toIntPr pr = case pr of
        [] -> (-1)
        _ -> read pr

getHtml :: URL -> IO (LBS.ByteString)
getHtml (URL url) = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest url
  response <- httpLbs request manager
  return $ responseBody response


printConcert :: Concert -> IO()
printConcert concert = do
  (putStrLn . title . concertInfo) concert
  (print . concertDate) concert
  case concertPrice concert of
    Nothing -> putStrLn "No tickects"
    Just pr -> print pr
  (putStrLn . concertPlace) concert
  let people = (persons . concertInfo) concert
  mapM_ (putStrLn . personName) people
  let rr pr = case personRole pr of
                Just x -> x
                Nothing -> ""
  mapM_ (putStrLn . rr) people
  mapM_ (print . personId) people

  (putStrLn . moreInfo . concertInfo) concert
  (print . urlAbout) concert
  putStrLn "-----------------------------------"


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
