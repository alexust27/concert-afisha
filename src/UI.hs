{-# LANGUAGE ScopedTypeVariables #-}
module UI where

import Control.Monad(void)
-- import Control.Monad.IO.Class
-- import Data.IORef
import Graphics.UI.Gtk
-- import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Builder(builderGetObject)
import Data.Char(isDigit)
import Database(getConcertsFromDB)
import Common (Concert(..), Info(..), Person(..), Price(..), getTimeAndDate, urlToStr)

-- hello :: (ButtonClass o) => o -> IO ()
-- hello b = set b [buttonLabel := "Hello World"]

concertToBox :: Concert -> IO Box
concertToBox concert = do
  builder2 <- builderNew
  builderAddFromFile builder2 "oneBlock.glade"
  concertBox      <- builderGetObject builder2 castToBox "one_concert_box"
  timeLabel       <- builderGetObject builder2 castToLabel "label_time"
  dateLabel       <- builderGetObject builder2 castToLabel "label_date"
  nameLabel       <- builderGetObject builder2 castToLabel "label_name"
  placeLabel      <- builderGetObject builder2 castToLabel "label_place"
  addInfoLabel    <- builderGetObject builder2 castToLabel "label_add_info"
  priceLabel      <- builderGetObject builder2 castToLabel "label_price"
  buttonBuy       <- builderGetObject builder2 castToLinkButton "linkbutton_buy"
  buttonAbout     <- builderGetObject builder2 castToLinkButton "linkbutton_about"
  infoTextBuffer  <- builderGetObject builder2 castToTextBuffer "textbuffer_info"

--   labelSetText Label str
--   textBufferSetText infoTextBuffer txtt

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

--   let people = persons infoC
--   mapM_ (putStrLn . personName) people
--   let rr pr = case personRole pr of
--                 Just x -> x
--                 Nothing -> ""
--   mapM_ (putStrLn . rr) people
--   mapM_ (print . personId) people


  return concertBox

gui :: IO ()
gui = do
  void initGUI
  window          <- windowNew
  set window [ windowTitle := "афиша", windowDefaultWidth := 800, windowDefaultHeight := 700]

  builder <- builderNew
  builderAddFromFile builder "afisha.glade"
  window2         <- builderGetObject builder castToWindow "main_window"
  searchButton    <- builderGetObject builder castToButton "search_button"
  searchEntry     <- builderGetObject builder castToEntry "search_entry"
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
--       (cYear, cMonth, cDay) <- calendarGetDate calendar
    minPrice   <- entryGetText minPriceEntry
    maxPrice   <- entryGetText maxPriceEntry
    let minPr :: Int = if null minPrice || not (all isDigit minPrice) then -1 else read minPrice
    let maxPr :: Int = if null maxPrice || not (all isDigit maxPrice) then -1 else read maxPrice
    concerts <- getConcertsFromDB searchText minPr maxPr cDate
    addedBlocks <- mapM concertToBox concerts
    mapM_ (containerAdd veiwBox) addedBlocks

  void $ searchButton `on` buttonActivated $ do

  widgetShowAll window
  void $ on window objectDestroy mainQuit
  mainGUI


--   onClicked button (hello button)
--   Just xml <- xmlNew "afishaMaket.glade"
--   window   <- xmlGetWidget xml castToWindow "window1"
--   onDestroy window mainQuit

--   window <- windowNew
--   set window [ windowTitle := "Афиша концертов",
--                containerBorderWidth := 10 ]
--
--   searchButton <- buttonNewWithLabel "Поиск"
--
--   set searchButton [ widgetHExpand := True ]
--
--   butBox <- hBoxNew True 0
--   containerAdd butBox searchButton
--
--   grid <- gridNew
--   gridAttach grid butBox 0 0 1 1
--
--   vbox <- vBoxNew False 0
--   containerAdd vbox grid
--
-- --   note <- notebookNew
--   containerAdd window vbox
--
--   windowMaximize window
--   widgetShowAll window
--
--




--   quitButton <- buttonNewWithLabel "Quit"
--   onClicked quitButton mainQuit
--   void $ window `on` deleteEvent $ liftIO mainQuit >> return False -- Закрытие окна



--   window <- windowNew
--   button <- buttonNew
--   set window [windowDefaultWidth := 800, windowDefaultHeight := 700,
--               containerChild := button, containerBorderWidth := 13]
--   onClicked button (hello button)
--   onDestroy window mainQuit
--   widgetShowAll window
--   mainGUI