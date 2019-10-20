{-# LANGUAGE ScopedTypeVariables #-}
module UI where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk
-- import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Builder
import Data.Char(isDigit)
import Database(getConcertsFromDB)
import Common (Concert(..), Info(..), Person(..),Price(..), Date(..), URL(..), getTimeAndDate, urlToStr)

hello :: (ButtonClass o) => o -> IO ()
hello b = set b [buttonLabel := "Hello World"]

concertToBox :: Concert -> IO Box
concertToBox concert = do
  builder2 <- builderNew
  builderAddFromFile builder2 "oneBlock.glade"
  concertBox      <- builderGetObject builder2 castToBox "one_concert_box"
  infoTextBuffer  <- builderGetObject builder2 castToTextBuffer "textbuffer_info"


--   textBufferSetText infoTextBuffer txtt

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

  searchButton `on` buttonActivated $ do
    searchText <- entryGetText searchEntry
    if null searchText then return ()
    else do
      putStrLn searchText
      entrySetText searchEntry ""
      cDate <- calendarGetDate calendar
--       (cYear, cMonth, cDay) <- calendarGetDate calendar
      minPrice   <- entryGetText minPriceEntry
      maxPrice   <- entryGetText maxPriceEntry
      let minPr :: Int = if null minPrice || not (all isDigit minPrice) then -1 else read minPrice
      let maxPr :: Int = if null maxPrice || not (all isDigit maxPrice) then -1 else read maxPrice

--       getConcertFromDB searchText minPr minPr
      concerts <- getConcertsFromDB searchText minPr maxPr cDate

      addedBlocks <- mapM concertToBox concerts
--       mainL <- labelNew $ Just "searchText"
--       labelSetLabel mainL "Цена\n 800-900 р."
      mapM_ (containerAdd veiwBox) addedBlocks

  widgetShowAll window
  on window objectDestroy mainQuit
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