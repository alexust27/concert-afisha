{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Database
( addConcertToDB
, createTables
, dropTables
) where

-- import Data.Time.Clock(UTCTime(..))
-- import Data.Time.Calendar(Day(..))
-- import Data.Time.LocalTime(TimeOfDay(..))
-- import Database.PostgreSQL.Simple.Time(parseDay, parseTimeOfDay)
-- import qualified Data.Text as T
-- import Control.Applicative
-- import Data.Int(Int64)

import Control.Monad (void)
import Database.PostgreSQL.Simple (Only(..), Connection, Query, connect, connectDatabase, connectPassword,
                                  connectUser, defaultConnectInfo, execute, execute_, executeMany, query)
import Database.PostgreSQL.Simple.SqlQQ (sql)

import Common (Concert(..), Info(..), Person(..),Price(..), getTimeAndDate, urlToStr)


take1fromListOnly :: [Only a] -> a
take1fromListOnly (x:_) = fromOnly x
take1fromListOnly [] = error "Can't take from List of Only"

updateOrInsertConcertInDB :: Connection -> Concert -> IO Int
updateOrInsertConcertInDB conn concert = do
  let cInfo = concertInfo concert
  let cName = head $ title $ cInfo
  let cPlace = concertPlace concert
  let (cTime, cDate) = getTimeAndDate $ concertDate concert
  let (minPrice, maxPrice) = case concertPrice concert of
                              Just (Price (prices, _)) -> (head prices, (prices !! 1))
                              Nothing -> (-1, -1)
  let addInfo = (unlines $ tail $ title $ cInfo) ++ "\n" ++ (unlines $ music cInfo) ++ "\n\n"
             ++ (unlines $ ansambles cInfo)
  let fullInfo = moreInfo cInfo
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
       max_price, add_info, more_info, ref_about)
      VALUES (?,?,?,?,?,?,?,?,?) returning id_concert
    |]
    concertDbId :: [Only Int] <- query conn qInsertInConcerts (cName, cPlace, cDate, cTime, minPrice,
                                                               maxPrice, addInfo, fullInfo, concertUrl)
    return $ take1fromListOnly concertDbId
  else do
    let cId = take1fromListOnly ids
    let qUpdateConcerts :: Query = [sql|
          UPDATE concerts SET
           name = ?, place = ?, concert_date = ?, concert_time = ?, min_price = ?,
           max_price = ?, add_info = ?, more_info = ?, ref_about = ?
          WHERE id_concert = ?
    |]
    void $ execute conn qUpdateConcerts (cName, cPlace, cDate, cTime, minPrice,
                                           maxPrice, addInfo, fullInfo, concertUrl, cId)
    return cId


insertArtist :: Connection -> Person -> IO Int
insertArtist conn artist = do
  let artistName = personName artist
  let artistRole = case personRole artist of
                    Just x -> x
                    _ -> ""
  let artistUrl = personId artist
  let qSelectArtists :: Query = [sql|
      SELECT id_artist FROM artists WHERE ref_about=? AND name=?
    |]
  artistDbId :: [Only Int] <- query conn qSelectArtists (artistUrl, artistName)
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
        max_price    integer,
        min_price    integer,
        add_info     text,
        more_info    text,
        ref_about    varchar(300) not null
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
    drop table artists;
    drop table concerts;
    drop table concert_artist;
  |]

  void $ execute_ conn qDrop


-- getByPrice :: Int -> Int