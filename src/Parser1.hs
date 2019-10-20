-- Парсер афиши Филармонии
module Parser1
  ( parseFun
  ) where

import Common ( Concert(..), Date(..), Info(..), Person(..), Price(..), TagClassName(..), URL(..), isClassInTag,
                fromTagToTag, getHtml, getFirstTextFromTag, getTextFromTags, strToPrice, urlToStr)

import Data.Maybe (catMaybes)
import qualified Data.List as L
import qualified Data.ByteString.Lazy.Char8 as LBS
import Text.HTML.TagSoup (Tag, (~/=),(~==), fromAttrib, parseTags, partitions)
toUtfString :: LBS.ByteString -> String
toUtfString = map (\ch -> case lookup ch rusTable of
                           Just x -> x
                           _ -> ch
                  ) . LBS.unpack
  where rusTable = zip (('\168'):('\184'):['\192'..'\255']) (('Ё'):('ё'):['А'..'я'])

getTextFromTagsUTF :: [Tag LBS.ByteString] -> String
getTextFromTagsUTF = getTextFromTags toUtfString

mainUrl :: URL
mainUrl = URL "https://www.philharmonia.spb.ru"

makeUrl :: Int -> Int -> Int -> URL
makeUrl d m y = URL $ (urlToStr mainUrl) ++ "/afisha/?ev_y=" ++ (show y) ++ "&ev_m=" ++ (show m) ++ "&ev_d=" ++ (show d)

parseDate :: Int -> Int -> [Tag LBS.ByteString] -> Date
parseDate m y block = Date { day = dayFromTag
                       , month = m
                       , year = y
                       , time = timeFromTag}
  where
    dayTag = TagClassName "date_day"
    timeTag = TagClassName "date_h"
    getFst = getFirstTextFromTag toUtfString block
    timeFromTag = getFst timeTag
    dayFromTag  = read $ getFst dayTag

parseInfoTags :: [Tag LBS.ByteString] -> Info
parseInfoTags tags = Info { title     = mainTitle
                          , addInfo   = aInfo
                          , moreInfo  = mInfo
                          , persons   = peoples
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

    mainTitle = getTextFromTagsUTF titleBlock
    mInfo     = if L.any (isClassInTag elementInfo) block
                then getTextFromTagsUTF infoBlock
                else ""
    peoples   = if L.any (isClassInTag personsTag) block
                then catMaybes $ map parsePeople $ partitions (isClassInTag (onePersonTag))
                               $ fromTagToTag personsTag ansamblesTag block
                else []
    curAnsambles = if L.any (isClassInTag ansambleTag) block
                   then "Ансамбли:\n" ++ (L.unlines $ map getTextFromTagsUTF $ take 3
                         $ partitions (isClassInTag ansambleTag) ansamblesBlock)
                   else []
    curMusic   = if L.any (isClassInTag musicianTag) block
                 then "Музыка:\n" ++ (L.intercalate ", " $ map getTextFromTagsUTF
                                 $ partitions (isClassInTag (musicianTag)) musicBlock)
                 else []
    aInfo      = curMusic ++ "\n" ++ curAnsambles


parsePeople :: [Tag LBS.ByteString] -> Maybe Person
parsePeople tags = if needSave then
                   Just Person { personName = pName
                               , personRole = Just pRole
                               , personId   = personRef
                               , isLiked    = False
                               }
                   else Nothing
  where
    block = fromTagToTag (TagClassName "nam") (TagClassName "rol") tags
    pName = getTextFromTagsUTF block
    pRole = getFirstTextFromTag toUtfString tags (TagClassName "rol")
    (personRef, needSave) = if L.any (~== "<a>") block
                then ((urlToStr mainUrl)
                      ++ (fromAttrib "href" $ toUtfString <$> (head $ dropWhile (~/= "<a>") block)), True)
                else ("", False)

parsePrice :: [Tag LBS.ByteString] -> Maybe Price
parsePrice tags = if L.any (isClassInTag pricesTag) tags
                  then Just $ Price (strToPrice priceBlock, URL ((urlToStr mainUrl) ++ urlToBuy))
                  else Nothing
  where
    toBuyTag = TagClassName "pts_btn btn_buy2"
    pricesTag = TagClassName "mer_item_prices"
    priceBlock = getFirstTextFromTag toUtfString tags pricesTag
    urlToBuy = fromAttrib "href" $ toUtfString <$> head (dropWhile (not . (isClassInTag toBuyTag)) tags)


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
                  (fromAttrib "href" $ toUtfString <$> head (dropWhile (not . (isClassInTag titleTag)) block))
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

parseFun :: Int -> Int -> Int -> IO [Concert]
parseFun d m y = do
  body <- getHtml (makeUrl d m y)
  let blocks = parseAfishaList body
  concerts <- mapM (parseOneBlock m y) $ take 8 blocks
  return concerts

