-- Парсер афиши Мариинского театра
module Parser2
  ( parseFun2
  ) where

import Common ( Concert(..), Date(..), Info(..), Person(..), Price(..), URL(..), urlToStr)
import ParsingUtils ( TagClassName(..), isClassInTag, fromTagToTag, getHtml,
                      getFirstTextFromTag, getTextFromTags, strToPrice )

import Data.Char (isSpace, chr)
import Data.Maybe (catMaybes)
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List as L
import qualified Data.Text as T
import Text.HTML.TagSoup (Tag, (~==), (~/=), isTagText, fromAttrib, fromTagText, parseTags, partitions)


getTextFromTagsUTF :: [Tag LBS.ByteString] -> String
getTextFromTagsUTF = getTextFromTags toString

mainUrl :: URL
mainUrl = URL "https://www.mariinsky.ru"


makeUrl :: Int -> Int -> URL
makeUrl m y = URL $ (urlToStr mainUrl) ++ "/ru/playbill/playbill/?year=" ++ (show y)
              ++ "&month=" ++ (show m)


parsePlace :: [Tag LBS.ByteString] -> String
parsePlace block = getTextFromTagsUTF $ take 2 $ dropWhile (~/= "<span itemprop=\"location\">") block


parseDate :: [Tag LBS.ByteString] -> Date
parseDate block = Date { day   = dayFromTag
                       , month = monthFromTag
                       , year  = yearFromTag
                       , time  = timeFromTag
                       }
  where
    timeBlock   = dropWhile (~/= "<div class=\"time\">") block
    timeFromTag = toString $ fromTagText $ timeBlock !! 2
    dateStr     = takeWhile (\ch -> ch /= 'T') $ fromAttrib "datetime" (toString <$> (timeBlock !! 1))
    [yearFromTag, monthFromTag, dayFromTag] = parseDateStr dateStr
    parseDateStr :: String -> [Int]
    parseDateStr str = map read $ L.words $ map (\ch -> if ch == '-' then ' ' else ch) str


parsePrice :: [Tag LBS.ByteString] -> IO (Maybe Price)
parsePrice block = do
  let tags     = fromTagToTag (TagClassName "t_button") (TagClassName "time2") block
  let toBuyTag = head tags
  if L.length tags > 1 && not (isClassInTag (TagClassName "no") toBuyTag)
  then do
    let urlFromTag         = (fromAttrib "href" $ toString <$> toBuyTag)
    let (urlToBuy, kassir) = if T.isInfixOf (T.pack ("spb.kassir.ru")) (T.pack urlFromTag)
                             then (URL urlFromTag, True)
                             else (URL $ "https:" ++ urlFromTag, False)
    priceHtml <- getHtml urlToBuy
    let priceTags = parseTags priceHtml
    if kassir
    then do
      let prices   = L.filter (\x -> x /= chr 160 && not (isSpace x))
                   $ getFirstTextFromTag toString priceTags (TagClassName "price")
      return $ Just $ Price (strToPrice prices, urlToBuy)
    else do
      let prices = map (getTextFromTagsUTF . (take 5)) $ partitions (~== "<tprice1>") priceTags
      let intPrices = map read prices
      let curPr  =  (minimum intPrices, maximum intPrices)
      return $ Just $ Price (curPr, urlToBuy)

  else return Nothing


getInfoUrl :: [Tag LBS.ByteString] -> URL
getInfoUrl block = URL $ (urlToStr mainUrl)
    ++ (fromAttrib "href" $ toString <$> ((dropWhile (not . (isClassInTag titleTag)) block) !! 1))
  where
    titleTag = TagClassName "spec_name"


parseInfoTags :: [Tag LBS.ByteString] -> Info
parseInfoTags tags = Info { title     = mainTitle
                          , addInfo   = addTitle
                          , moreInfo  = mInfo
                          , persons   = peoples
                          }
  where
    block = takeWhile (~/= "<div id=\"soc_net\">") $ dropWhile (~/= "<div id=\"spec_info_container\">") tags
    titleBlocks = filter isTagText $ dropWhile (~/="<span itemprop=\"summary\">") tags
    mainTitle   = getTextFromTagsUTF [(titleBlocks !! 0)]
    addTitle    = getTextFromTagsUTF [(titleBlocks !! 1)]
    mInfo       = getTextFromTagsUTF block
    peoples     = if L.any (~== "<a>") block
                  then catMaybes $ map parsePeople $ partitions (~== "<a>") block
                  else []


parsePeople :: [Tag LBS.ByteString] -> Maybe Person
parsePeople tags = if L.length (filter (isSpace) pName) > 2
                   then Nothing
                   else Just $ Person { personName     = pName
                                      , personRole     = Nothing
                                      , personId       = personRef
                                      , isLiked        = False
                                      }
                   where
                     pName     = getTextFromTagsUTF $ take 3 tags
                     personRef = (urlToStr mainUrl) ++ (fromAttrib "href" $ toString <$> (head tags))


parseOneConcertBlock :: [Tag LBS.ByteString] -> IO (Concert)
parseOneConcertBlock block = do
  let onePlace  =  parsePlace block
  let oneDate   =  parseDate block
  onePrice      <- parsePrice block
  let infoUrl   =  getInfoUrl block
  infos         <- getHtml infoUrl
  let allInfo   =  (parseInfoTags . parseTags) infos
  let concert   =  Concert { concertDate   = oneDate
                           , concertPlace  = onePlace
                           , concertPrice  = onePrice
                           , concertInfo   = allInfo
                           , urlAbout      = infoUrl
                           }
  return concert


parseBigBlock :: [Tag LBS.ByteString] -> IO ([Concert])
parseBigBlock block = do
  let blockTag    = TagClassName "col-md-10 spects"
  let rowTag      = TagClassName "row "
  let smallBlocks = partitions (isClassInTag rowTag) $ drop 1
                  $ dropWhile(not . (isClassInTag blockTag)) block
  mapM parseOneConcertBlock smallBlocks


parseAfishaList :: LBS.ByteString -> [[Tag LBS.ByteString]]
parseAfishaList body = partitions (isClassInTag oneItemTag) mainContent
  where
    lastContentTag = TagClassName "row playbill_filter_bottom"
    oneItemTag     = TagClassName "day_row"
    tagList        = parseTags body :: [Tag LBS.ByteString]
    mainContent :: [Tag LBS.ByteString]
    mainContent = takeWhile (not . (isClassInTag lastContentTag))
                $ dropWhile (~/= "<div id=\"afisha\">") tagList

parseFun2 :: Int -> Int -> Int -> IO ([Concert])
parseFun2 d m y = do
  body <- getHtml (makeUrl m y)
  let bigBlocks = parseAfishaList body
  res <- mapM parseBigBlock $ take 3 $ drop (d - 1) bigBlocks
  return $ L.concat res
