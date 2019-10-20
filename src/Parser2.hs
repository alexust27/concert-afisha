-- Парсер афиши Мариинского театра
module Parser2
  ( parseFun2
  ) where

import Common ( Concert(..), Date(..), Info(..), Person(..), Price(..), TagClassName(..), URL(..), isClassInTag,
                fromTagToTag, getHtml, getTextFromTags, getFirstTextFromTag, strToPrice, urlToStr)

import Data.Char (isDigit, isSpace, chr)
import Data.Maybe (catMaybes)
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List as L
import qualified Data.Text as T
import Debug.Trace(trace)
import Text.HTML.TagSoup (Tag, (~==), (~/=), isTagText, fromAttrib, fromTagText, parseTags, partitions)


getTextFromTagsUTF :: LBS.ByteString -> String
getTextFromTagsUTF = getTextFromTags toString

mainUrl :: URL
mainUrl = URL "https://www.mariinsky.ru"

makeUrl :: Int -> Int -> URL
makeUrl m y = URL $ (urlToStr mainUrl) ++ "/ru/playbill/playbill/?year=" ++ (show y) ++ "&month=" ++ (show m)


parsePlace :: [Tag LBS.ByteString] -> String
parsePlace block = getTextFromTagsUTF $ take 2 $ dropWhile (~/= "<span itemprop=\"location\">") block


parseDate :: [Tag LBS.ByteString] -> Date
parseDate block = Date {day = dayFromTag, month = monthFromTag, year = yearFromTag, time = timeFromTag}
  where
    timeBlock   = (dropWhile (~/= "<div class=\"time\">") block )
    timeFromTag = LBS.unpack $ fromTagText $ timeBlock !! 2
    dateStr     = takeWhile (\ch -> ch /= 'T') $ fromAttrib "datetime" (LBS.unpack <$> (timeBlock !! 1))
    [yearFromTag, monthFromTag, dayFromTag] = parseDateStr dateStr
    parseDateStr :: String -> [Int]
    parseDateStr str = map read $ L.words $ map (\ch -> if ch == '-' then ' ' else ch) str


parsePrice :: [Tag LBS.ByteString] -> IO (Maybe Price)
parsePrice block = do
  let tags     = fromTagToTag (TagClassName "t_button") (TagClassName "time2") block
  let toBuyTag = head tags
  if L.length tags > 1 && not (isClassInTag (TagClassName "no") toBuyTag)
  then do
    let urlFromTag         = (fromAttrib "href" $ LBS.unpack <$> toBuyTag)
    let (urlToBuy, kassir) = if T.isInfixOf (T.pack ("spb.kassir.ru")) (T.pack urlFromTag)
                             then (URL urlFromTag, True)
                             else (URL $ "https:" ++ urlFromTag, False)
    priceHtml <- getHtml urlToBuy
    let priceTags = parseTags priceHtml
    let prices    = getFirstFromTags partitions (\x -> x ~== "<tprice1>" || x ~== "<tprice2>"
                  || x ~== "<div class=\"price\" data-ajaxupdateable=\"price\">") priceTags
    if kassir
    then do
      let priceTxt = L.filter (\x -> not (isSpace x) && x /= chr 160) $ getTextFromTagsUTF prices
      let priceS =trace ("pr=" ++ show priceTxt) [read $ L.takeWhile isDigit priceTxt,
                                  read (L.filter isDigit $ L.dropWhile isDigit priceTxt)]
      return $ Just $ Price (priceS, urlToBuy)
    else
      return $ Just $ Price (strToPrice (L.unwords prices), urlToBuy)

  else return Nothing


getInfoUrl :: [Tag LBS.ByteString] -> URL
getInfoUrl block = URL $ (urlToStr mainUrl)
    ++ (fromAttrib "href" $ LBS.unpack <$> ((dropWhile (not . (isClassInTag titleTag)) block) !! 1))
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
    mainTitleD = getTextFromTags [(titleBlocks !! 0)]
    tr = trace ("title=" ++ show mainTitleD) mainTitleD
    mainTitle = toString tr
    addTitle  = toString $ getTextFromTags [(titleBlocks !! 1)]
    mInfo     = toString $ getTextFromTags block
    peoples   = if L.any (~== "<a>") block
                then catMaybes $ map parsePeople $ partitions (~== "<a>") block
                else []


parsePeople :: [Tag LBS.ByteString] -> Maybe Person
parsePeople tags = if L.length maybePerson > 2
                   then Nothing
                   else Just $ Person { personName     = pName
                                      , personRole     = Nothing
                                      , personId       = personRef
                                      , isLiked        = False
                                      }
                   where
                     maybePerson = LBS.words $ getTextFromTags $ take 3 tags
                     pName       = toString $ LBS.unwords maybePerson
                     personRef   = (urlToStr mainUrl) ++ (fromAttrib "href" $ LBS.unpack <$> (head tags))


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



-- testUrl = URL "https://www.mariinsky.ru/playbill/playbill?place=theatre"

parseFun2 :: Int -> Int -> IO ([Concert])
parseFun2 m y = do
  body <- getHtml (makeUrl m y)
  let bigBlocks = parseAfishaList body
  res <- mapM parseBigBlock $ take 1 bigBlocks
  return $ L.concat res


--     parseBigBlock $ head bigBlocks
--     mapM_ parseBigBlock bigBlocks
--   parseBigBlock $ head bigBlocks

--         infos <- sequence $ map getHtml infoUrls

    --     print blocks
--     LBS.putStrLn $ LBS.intercalate (LBS.pack "#####") $ map LBS.unlines res

--
--     let dates = map parseDate blocks
--     let prices = map parsePrice blocks
--     let infoUrls = map getInfoUrl blocks
--     infos <- sequence $ map getHtml infoUrls
--     let allInfo = map (parseInfoTags . parseTags) infos
--     let concerts = [Concert {concertDate = x, concertPrice = y, concertInfo = z }
--                     | (x, y, z) <- zip3 dates prices allInfo]
--     return concerts


--             print prices
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


