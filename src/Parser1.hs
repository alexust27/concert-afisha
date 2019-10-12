-- Парсер афиши Филармонии
module Parser1
  ( parseFun
  ) where

import Common ( Concert(..), Date(..), Info(..), Person(..), Price(..), TagClassName(..), URL(..), isClassInTag,
                fromTagToTag, getHtml, getIntFromTag, getOneElemFromTags, getTextFromTags, strToPrice, urlToStr)

import Data.Maybe (catMaybes)
import qualified Data.List as L
import qualified Data.ByteString.Lazy.Char8 as LBS
import Text.HTML.TagSoup (Tag, (~/=),(~==), fromAttrib, parseTags, partitions)


toUtfString :: LBS.ByteString -> String
toUtfString = map (\chr -> case lookup chr rusTable of
                           Just x -> x
                           _ -> chr
                  ) . LBS.unpack
  where rusTable = zip (('\168'):('\184'):['\192'..'\255']) (('Ё'):('ё'):['А'..'я'])


mainUrl :: URL
mainUrl = URL "https://www.philharmonia.spb.ru"

makeUrl :: Int -> Int -> URL
makeUrl m y = URL $ (urlToStr mainUrl) ++ "/afisha/?ev_y=" ++ (show y) ++ "&ev_m=" ++ (show m)

parseDate :: Int -> Int -> [Tag LBS.ByteString] -> Date
parseDate m y block = Date { day = dayFromTag
                       , month = m
                       , year = y
                       , time = timeFromTag}
  where
    dayTag = TagClassName "date_day"
    timeTag = TagClassName "date_h"
    timeFromTag = LBS.unpack $ getOneElemFromTags block timeTag 0
    dayFromTag = getIntFromTag block dayTag

parseInfoTags :: [Tag LBS.ByteString] -> Info
parseInfoTags tags = Info { title     = mainTitle
                          , moreInfo  = mInfo
                          , persons   = peoples
                          , ansambles = curAnsambles
                          , music     = curMusic
                          }
  where
    elementAge   = TagClassName "afisha_element_age"
    titleTag     = TagClassName "afisha_element_title"
    elementInfo  = TagClassName "afisha_element_dett"
    personsTag   = TagClassName "ae_persons_main"
    onePersonTag = TagClassName "ae_persons_maini ta"
    ansamblesTag = TagClassName "ansambles"
    ansambleTag  = TagClassName "ansamble_title"
    musicTag     = TagClassName "td ae_music"
    musicianTag  = TagClassName "ae_music_auithor_o"

    block           = fromTagToTag (TagClassName "td right") (TagClassName "afisha_element_slider_next") tags
    titleBlock      = fromTagToTag titleTag elementAge block
    infoBlock       = fromTagToTag elementInfo personsTag block
    ansamblesBlock  = fromTagToTag ansamblesTag musicTag block
    musicBlock      = dropWhile (not . (isClassInTag musicTag)) block

    mainTitle = [toUtfString $ getTextFromTags titleBlock]
    mInfo     = if L.any (isClassInTag elementInfo) block
                then toUtfString $ getTextFromTags infoBlock
                else ""
    peoples   = if L.any (isClassInTag personsTag) block
                then catMaybes $ map parsePeople $ partitions (isClassInTag (onePersonTag))
                               $ fromTagToTag personsTag ansamblesTag block
                else []
    curAnsambles = if L.any (isClassInTag ansambleTag) block
                   then map (toUtfString . getTextFromTags) $ take 3
                        $ partitions (isClassInTag ansambleTag) ansamblesBlock
                   else []
    curMusic     = if L.any (isClassInTag musicianTag) block
                   then map (toUtfString . getTextFromTags) $ partitions (isClassInTag (musicianTag)) musicBlock
                   else []


parsePeople :: [Tag LBS.ByteString] -> Maybe Person
parsePeople tags = if needSave then
                   Just Person { name = personName
                               , role = personRole
                               , personId = personRef
                               }
                   else Nothing
  where
    block      = fromTagToTag (TagClassName "nam") (TagClassName "rol") tags
    personName = toUtfString $ getTextFromTags block
    personRole = toUtfString $ getTextFromTags $ dropWhile (not . (isClassInTag (TagClassName "rol"))) tags
    (personRef, needSave) = if L.any (~== "<a>") block
                then ((urlToStr mainUrl)
                      ++ (fromAttrib "href" $ LBS.unpack <$> (head $ dropWhile (~/= "<a>") block)), True)
                else ("", False)

parsePrice :: [Tag LBS.ByteString] -> Maybe Price
parsePrice tags = if L.any (isClassInTag pricesTag) tags
                  then Just $ Price (strToPrice (getTextFromTags priceBlock), URL ((urlToStr mainUrl) ++ urlToBuy))
                  else Nothing
  where
    toBuyTag = TagClassName "pts_btn btn_buy2"
    pricesTag = TagClassName "mer_item_prices"
    priceBlock = take 2 $ dropWhile (not . (isClassInTag pricesTag)) tags
    urlToBuy = fromAttrib "href" $ LBS.unpack <$> head (dropWhile (not . (isClassInTag toBuyTag)) tags)


parseAfishaList :: LBS.ByteString -> [[Tag LBS.ByteString]]
parseAfishaList body = partitions (isClassInTag oneItemTag) $ mainContent tagList
  where
    startContentTag = TagClassName "afisha_list_items"
    lastContentTag = TagClassName "bottom_sponsor"
    oneItemTag = TagClassName "afisha_list_item zal"
    tagList = parseTags body
    mainContent = fromTagToTag startContentTag lastContentTag


getInfoUrl :: [Tag LBS.ByteString] -> URL
getInfoUrl block = URL $ (urlToStr mainUrl) ++
                  (fromAttrib "href" $ LBS.unpack <$> head (dropWhile (not . (isClassInTag titleTag)) block))
                where
                  titleTag = TagClassName "mer_item_title hand"

parseOneBlock :: Int -> Int -> [Tag LBS.ByteString] -> IO Concert
parseOneBlock m y block = do
  let onePrice  = parsePrice block
  let oneDate   = parseDate m y block
  let infoUrl   = getInfoUrl block
  infos <- getHtml infoUrl
  let allInfo = (parseInfoTags . parseTags) infos
  let concert = Concert { concertDate   = oneDate
                        , concertPrice  = onePrice
                        , concertInfo   = allInfo
                        , concertPlace  = "Филармония"
                        , urlAbout      = infoUrl
                        }
  return concert

-- testUrl = URL "https://www.philharmonia.spb.ru/afisha/323968/"
-- testUrl = URL "https://www.philharmonia.spb.ru/afisha/"

parseFun :: Int -> Int -> IO [Concert]
parseFun m y = do
  body <- getHtml (makeUrl m y)
  let blocks = parseAfishaList body
  concerts <- mapM (parseOneBlock m y) blocks
  return concerts

