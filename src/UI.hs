{-# LANGUAGE ScopedTypeVariables #-}
module UI
( startGui
) where

import Control.Monad (void)
import Data.Char (isDigit)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder (builderGetObject)

import Common (Concert(..), Info(..), Person(..), Price(..), getTimeAndDate, urlToStr)
import Database (getConcertsFromDB, updateByDate)


concertToBox :: Concert -> IO Box
concertToBox concert = do
  builder2 <- builderNew
  builderAddFromFile builder2 "maket/oneBlock.glade"
  concertBox      <- builderGetObject builder2 castToBox "one_concert_box"
  artistsBox      <- builderGetObject builder2 castToBox "box_artists"
  timeLabel       <- builderGetObject builder2 castToLabel "label_time"
  dateLabel       <- builderGetObject builder2 castToLabel "label_date"
  nameLabel       <- builderGetObject builder2 castToLabel "label_name"
  placeLabel      <- builderGetObject builder2 castToLabel "label_place"
  addInfoLabel    <- builderGetObject builder2 castToLabel "label_add_info"
  priceLabel      <- builderGetObject builder2 castToLabel "label_price"
  buttonBuy       <- builderGetObject builder2 castToLinkButton "linkbutton_buy"
  buttonAbout     <- builderGetObject builder2 castToLinkButton "linkbutton_about"
  infoTextBuffer  <- builderGetObject builder2 castToTextBuffer "textbuffer_info"

  let infoC = concertInfo concert
  labelSetText nameLabel (title infoC)
  let cDate = concertDate concert
  let (strTime, strDate) = getTimeAndDate cDate
  labelSetText dateLabel strDate
  labelSetText timeLabel $ take 5 strTime
  let priceToStr p1 p2 = (if p1 == p2 then show p1 else (show p1) ++ "-" ++ (show p2) ) ++ " р."
  let (prices, urlBuy) = case concertPrice concert of
                          Nothing -> ("Билетов нет", "")
                          Just (Price((p1, p2), url)) -> (priceToStr p1 p2, urlToStr url)
  labelSetText priceLabel prices

  labelSetText placeLabel $ concertPlace concert
  labelSetText addInfoLabel $ addInfo infoC
  textBufferSetText infoTextBuffer $ moreInfo infoC

  set buttonAbout [linkButtonURI := (urlToStr $ urlAbout concert) ]
  set buttonBuy   [linkButtonURI := urlBuy ]

  let people = persons infoC
  print (length people)
  personBoxes <- mapM personToBox people
  print (length personBoxes)
  mapM_ (containerAdd artistsBox) personBoxes
  return concertBox

personToBox :: Person -> IO Box
personToBox p = do
  builder3 <- builderNew
  builderAddFromFile builder3 "maket/onePerson.glade"
  box <- builderGetObject builder3 castToBox "box1"
  b <- builderGetObject builder3 castToToggleButton "togglebutton1"
  labl <- builderGetObject builder3 castToLabel "person1"

  let pn = personName p
  let liked = isLiked p
  labelSetText labl pn
  toggleButtonSetActive b liked
  return box



startGui :: IO ()
startGui = do
  void initGUI
  window <- windowNew
  set window [ windowTitle := "афиша", windowDefaultWidth := 1200, windowDefaultHeight := 700]

  builder <- builderNew
  builderAddFromFile builder "maket/afisha.glade"
  window2         <- builderGetObject builder castToWindow "main_window"
  searchButton    <- builderGetObject builder castToButton "search_button"
  searchEntry     <- builderGetObject builder castToEntry "search_entry"
  daysEntry       <- builderGetObject builder castToEntry "days_entry"
  calendar        <- builderGetObject builder castToCalendar "calendar1"
  minPriceEntry   <- builderGetObject builder castToEntry "min_price_entry"
  maxPriceEntry   <- builderGetObject builder castToEntry "max_price_entry"
  updateButton    <- builderGetObject builder castToButton "update_button"
  veiwBox         <- builderGetObject builder castToBox "box_view"
  mainBox         <- builderGetObject builder castToBox "box1"

  containerRemove window2 mainBox
  containerAdd window mainBox

  void $ searchButton `on` buttonActivated $ do
    searchText <- entryGetText searchEntry
    childsBox <- containerGetChildren veiwBox
    mapM_ (containerRemove veiwBox) childsBox
    putStrLn searchText
    entrySetText searchEntry ""
    cDate <- calendarGetDate calendar
    minPrice   <- entryGetText minPriceEntry
    maxPrice   <- entryGetText maxPriceEntry
    daysE      <- entryGetText daysEntry
    let minPr :: Int = if null minPrice || not (all isDigit minPrice) then -1 else read minPrice
    let maxPr :: Int = if null maxPrice || not (all isDigit maxPrice) then -1 else read maxPrice
    let days  :: Int = if null daysE    || not (all isDigit daysE)    then 1  else read daysE
    concerts <- getConcertsFromDB searchText days minPr maxPr cDate
    addedBlocks <- mapM concertToBox concerts
    mapM_ (containerAdd veiwBox) addedBlocks

  void $ updateButton `on` buttonActivated $ do
      (cYear, cMonth, cDay) <- calendarGetDate calendar
      updateByDate cDay (cMonth + 1) cYear

  widgetShowAll window
  void $ on window objectDestroy mainQuit
  mainGUI

