-- Общие функции для парсинга сайтов
module ParsingUtils
( TagClassName(..)
, isClassInTag
, fromTagToTag
, getHtml
, getFirstTextFromTag
, getTextFromTags
, strToPrice
) where

import Network.HTTP.Client (httpLbs, newManager, parseRequest, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Text.HTML.TagSoup (Tag, Tag ( TagOpen ), isTagText, fromAttrib, fromTagText)
import Data.Char(isDigit, chr)
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.ByteString.Lazy.Char8 as LBS
import Common (URL(..))


newtype TagClassName = TagClassName String

isClassInTag :: TagClassName -> Tag LBS.ByteString -> Bool
isClassInTag (TagClassName className) tag@(TagOpen _ _) = T.isInfixOf (T.pack className) $ T.pack
                                                        $ fromAttrib "class" $ LBS.unpack <$> tag
isClassInTag _ _ = False

-- возвращает список тегов с тега с классом "start" до тега с классом "end" не включая их
fromTagToTag :: TagClassName -> TagClassName -> [Tag LBS.ByteString] -> [Tag LBS.ByteString]
fromTagToTag start end tags = takeWhile (not . (isClassInTag end))
                            $ tail $ dropWhile (not . (isClassInTag start)) tags

-- возвращает текст из блока тегов
getTextFromTags :: (LBS.ByteString -> String) -> [Tag LBS.ByteString] -> String
getTextFromTags toStrFun tags = delSpaces $ map (changeSim . toStrFun . fromTagText) $ filter isTagText tags
  where
    delSpaces = L.unwords . L.words . L.unwords
    changeSim = (map (\x -> if x == chr 65533 then ' ' else x))


-- возвращает первый текст после тега с данным классом
getFirstTextFromTag :: (LBS.ByteString -> String) -> [Tag LBS.ByteString] -> TagClassName -> String
getFirstTextFromTag toStrF block className =  if L.null res then "" else toNormalText $ head res
  where
    res = dropWhile (not . isTagText) $ dropWhile (not . (isClassInTag className)) block
    toNormalText = L.unwords . L.words . toStrF . fromTagText

-- берет превые два числа из строки
strToPrice :: String -> (Int, Int)
strToPrice str = (mnPrice, max mxPrice mnPrice)
  where
    clearStr = L.unwords . L.words $ str
    mnPrice = toIntPr $ takeWhile (isDigit) clearStr
    mxPrice = toIntPr $ takeWhile (isDigit) $ dropWhile (not . isDigit) $ dropWhile (isDigit) clearStr
    toIntPr :: String -> Int
    toIntPr pr = case pr of
        "" -> (-1)
        _ -> read pr

getHtml :: URL -> IO (LBS.ByteString)
getHtml (URL url) = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest url
  response <- httpLbs request manager
  return $ responseBody response
