{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Database
( getConcertsFromDB
, updateByDate
) where

import Control.Monad (void)
import Data.Time.Calendar(Day(..), toGregorian)
import Data.Time.LocalTime(TimeOfDay(..))
import Database.PostgreSQL.Simple (Only(..), Connection, Query, connect, connectDatabase, connectPassword,
                                  connectUser, defaultConnectInfo, execute, execute_, executeMany, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Common (Concert(..), Info(..), Person(..), Price(..), Date(..), URL(..), getTimeAndDate, urlToStr)
import Parser1 (parseFun)
import Parser2 (parseFun2)

updateByDate :: Int -> Int -> Int -> IO ()
updateByDate d m y = do
--   dropTables
--   createTables
  concerts <- parseFun d m y
  concerts2 <- parseFun2 d m y

  mapM_ addConcertToDB concerts
  mapM_ addConcertToDB concerts2


take1fromListOnly :: [Only a] -> a
take1fromListOnly (x:_) = fromOnly x
take1fromListOnly [] = error "Can't take from List of Only"

updateOrInsertConcertInDB :: Connection -> Concert -> IO Int
updateOrInsertConcertInDB conn concert = do
  let cInfo = concertInfo concert
  let cName = title cInfo
  let cPlace = concertPlace concert
  let (cTime, cDate) = getTimeAndDate $ concertDate concert
  let (minPrice, maxPrice, urlBuy) = case concertPrice concert of
                              Just (Price ((price1, price2), URL url)) -> (price1, price2, url)
                              Nothing -> (-1, -1, "")
  let cAddInfo   = addInfo cInfo
  let fullInfo   = moreInfo cInfo
  let concertUrl = urlToStr $ urlAbout concert

  let qSel = [sql|
      SELECT id_concert FROM concerts
      WHERE name=? AND place=? AND concert_date=? AND concert_time=?
  |]

  let concertArgs = (cName, cPlace, cDate, cTime)
  ids :: [Only Int] <- query conn qSel concertArgs
  if null ids then do
    let qInsertInConcerts :: Query = [sql|
      INSERT INTO concerts
      (name, place, concert_date, concert_time, min_price,
       max_price, add_info, more_info, ref_about, ref_buy)
      VALUES (?,?,?,?,?,?,?,?,?,?) returning id_concert
    |]
    concertDbId :: [Only Int] <- query conn qInsertInConcerts (cName, cPlace, cDate, cTime, minPrice,
                                                               maxPrice, cAddInfo, fullInfo, concertUrl, urlBuy)
    return $ take1fromListOnly concertDbId
  else do
    let cId = take1fromListOnly ids
    let qUpdateConcerts :: Query = [sql|
          UPDATE concerts SET
           name = ?, place = ?, concert_date = ?, concert_time = ?, min_price = ?,
           max_price = ?, add_info = ?, more_info = ?, ref_about = ?, ref_buy = ?
          WHERE id_concert = ?
    |]
    void $ execute conn qUpdateConcerts (cName, cPlace, cDate, cTime, minPrice, maxPrice,
                                         cAddInfo, fullInfo, concertUrl, urlBuy, cId)
    return cId


insertArtist :: Connection -> Person -> IO Int
insertArtist conn artist = do
  let artistName = personName artist
  let artistRole = case personRole artist of
                    Just x -> x
                    _ -> ""
  let artistUrl = personId artist
  let qSelectArtists :: Query = [sql|
      SELECT id_artist FROM artists WHERE ref_about=?
    |]
  artistDbId :: [Only Int] <- query conn qSelectArtists (Only artistUrl)
  if null artistDbId then do
    let qInsertInArtists :: Query = [sql|
      INSERT INTO artists (name, role, ref_about) VALUES (?,?,?) returning id_artist
    |]
    artistDbId2 :: [Only Int] <- query conn qInsertInArtists (artistName, artistRole, artistUrl)
    return $ take1fromListOnly artistDbId2
  else do
    let artistId = take1fromListOnly artistDbId
    let qUpdateArtists :: Query = [sql|
          UPDATE artists
            SET role = ?
          WHERE id_artist = ?
    |]
    void $ execute conn qUpdateArtists (artistRole, artistId)
    return artistId


addConcertToDB :: Concert -> IO ()
addConcertToDB concert = do
  conn <- connect defaultConnectInfo {
    connectUser = "postgres",
    connectPassword = "123",
    connectDatabase = "haskell"
  }

  concertId :: Int <- updateOrInsertConcertInDB conn concert

  let qDelArtists :: Query = [sql|
    DELETE FROM concert_artist
      WHERE id_concert=?
   |]
  void $ execute conn qDelArtists (Only concertId)

  let artists = persons $ concertInfo concert
  if not (null artists) then do
    artistDbIds <- mapM (insertArtist conn) artists
    let qInsertCommon :: Query = [sql|
      INSERT INTO concert_artist
      (id_concert, id_artist) VALUES (?,?)
     |]
    let listToEx :: [(Int, Int)] = zip (repeat concertId) artistDbIds
    void $ executeMany conn qInsertCommon listToEx
  else return ()

createTables :: IO()
createTables = do
  conn <- connect defaultConnectInfo {
      connectUser = "postgres",
      connectPassword = "123",
      connectDatabase = "haskell"
  }

  let qCreateArtists = [sql|
    create table if not exists artists
    (
        id_artist serial not null
            constraint artists_pk
                primary key,
        name      varchar(100),
        ref_about varchar(300),
        liked     boolean default false not null,
        role      varchar(200)
    );
    alter table artists
        owner to postgres;
    create unique index artists_ref_about_uindex
        on artists (ref_about);
  |]
  let qCreateConcerts = [sql|
    create table if not exists concerts
    (
        id_concert   serial       not null
            constraint test1_pk
                primary key,
        name         varchar(300) not null,
        place        varchar(100) not null,
        concert_date date         not null,
        concert_time time         not null,
        min_price    integer,
        max_price    integer,
        add_info     text,
        more_info    text,
        ref_about    varchar(300) not null,
        ref_buy      varchar(300)
    );
    alter table concerts
        owner to postgres;
  |]
  let qCreateCommon = [sql|
    create table if not exists concert_artist
    (
        id_artist  integer,
        id_concert integer
    );
    alter table concert_artist
        owner to postgres;
  |]

  mapM_ (execute_ conn) [qCreateConcerts, qCreateArtists, qCreateCommon]


dropTables :: IO ()
dropTables = do
  conn <- connect defaultConnectInfo {
        connectUser = "postgres",
        connectPassword = "123",
        connectDatabase = "haskell"
  }

  let qDrop = [sql|
    drop table IF EXISTS artists;
    drop table IF EXISTS concerts;
    drop table IF EXISTS concert_artist;
  |]

  void $ execute_ conn qDrop

qArtistToPerson :: (String, Bool, String) -> Person
qArtistToPerson (pName, liked, reff) = Person { personName  = pName
                                              , personRole  = Nothing
                                              , personId    = reff
                                              , isLiked     = liked
                                              }

qToConcert :: Connection -> (Int, String, String, Day, TimeOfDay, Int, Int, String, String, String, String) -> IO Concert
qToConcert conn (concertId, cName, cPlace, cDay, cTime, cMinPrice, cMaxPrice, cAddInfo, qMoreInfo, refInfo, refBuy) = do
  let (yr, m, d) = toGregorian cDay
  let oneDate = Date { day   = d
                     , month = m
                     , year  = fromInteger yr
                     , time  = show cTime
                     }

  let onePrice = if cMinPrice == -1
                 then Nothing
                 else Just $ Price ((cMinPrice, cMaxPrice), (URL refBuy))

  let qSel = [sql|
        SELECT artists.name, artists.liked, artists.ref_about FROM artists INNER JOIN concert_artist
        ON artists.id_artist = concert_artist.id_artist AND concert_artist.id_concert = ?
  |]
  qArtists :: [(String, Bool, String)] <- query conn qSel (Only concertId)
  let artists = map qArtistToPerson qArtists
  let allInfo = Info { title      = cName
                     , addInfo    = cAddInfo
                     , moreInfo   = qMoreInfo
                     , persons    = artists
                     }

  return Concert { concertDate   = oneDate
                 , concertPrice  = onePrice
                 , concertInfo   = allInfo
                 , concertPlace  = cPlace
                 , urlAbout      = URL refInfo
                 }


getConcertsFromDB :: String -> Int -> Int -> Int -> (Int, Int, Int) -> IO [Concert]
getConcertsFromDB searchText days minPrice maxPrice cDate = do
  conn <- connect defaultConnectInfo {
        connectUser = "postgres",
        connectPassword = "123",
        connectDatabase = "haskell"
  }
  let price1 = if minPrice == -1 then -1 else minPrice
  let price2 = if maxPrice == -1 then 10000000 else maxPrice
  let qDate1 = case cDate of
        (ya, m, d) -> show d ++ "-" ++ show (m + 1) ++ "-" ++ show ya
  let qDate2 = case cDate of
        (ya, m, d) -> show (d + days) ++ "-" ++ show (m + 1) ++ "-" ++ show ya
  putStrLn ("try to get conc: " ++ searchText ++ " date=" ++ qDate1 )

  let qSel = [sql|
      SELECT * FROM concerts
      WHERE ? <= max_price AND min_price <= ? AND ? <= concert_date AND concert_date < ?
        AND (name SIMILAR TO '%' || ? || '%'  OR more_info SIMILAR TO '%' || ? || '%')
      ORDER BY concert_date ASC, concert_time ASC, min_price ASC, max_price ASC
  |]
  qConerts <- query conn qSel (price1, price2, qDate1, qDate2, searchText, searchText)

  concc <- mapM (qToConcert conn) qConerts
  return concc
