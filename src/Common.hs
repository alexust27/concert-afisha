module Common
( Concert(..)
, Date(..)
, Info(..)
, Person(..)
, Price(..)
, URL(..)
, getTimeAndDate
, printConcert
, urlToStr
) where


newtype URL = URL String deriving (Show, Eq)
data Date = Date { day    :: Int
                 , month  :: Int
                 , year   :: Int
                 , time   :: String
                 } deriving (Eq, Show)

data Price = Price ( (Int, Int)
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


getTimeAndDate :: Date -> (String, String)
getTimeAndDate d = (time d, dat)
  where
    dat = show (day d) ++ "-" ++ show (month d) ++ "-" ++ show (year d)

urlToStr :: URL -> String
urlToStr (URL str) = str


printConcert :: Concert -> IO()
printConcert concert = do

  let infoC = concertInfo concert
  (putStrLn . title) infoC
  (print . concertDate) concert
  putStrLn "Билеты:"
  case concertPrice concert of
    Nothing -> putStrLn "No tickects"
    Just pr -> print pr
  putStrLn "Место:"
  (putStrLn . concertPlace) concert
  putStrLn "Люди:"
  let people = persons infoC
  mapM_ (putStrLn . personName) people
  let rr pr = case personRole pr of
                Just x -> x
                Nothing -> ""
  putStrLn "Роли:"
  mapM_ (putStrLn . rr) people
  putStrLn "ссылки на людей:"
  mapM_ (print . personId) people
  putStrLn "Подробнее:"
  (putStrLn . addInfo) infoC
  putStrLn "-----"
  (putStrLn . moreInfo) infoC
  (print . urlAbout) concert
  putStrLn "-----------------------------------"
