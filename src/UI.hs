{-# LANGUAGE ScopedTypeVariables #-}
module UI where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk
-- import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Builder

hello :: (ButtonClass o) => o -> IO ()
hello b = set b [buttonLabel := "Hello World"]

gui :: IO ()
gui = do
  void initGUI
  builder <- builderNew
  builderAddFromFile builder "afisha.glade"
--   window          <- builderGetObject builder castToWindow "main_window"
  searchButton    <- builderGetObject builder castToButton "search_button"
  searchEntry     <- builderGetObject builder castToEntry "search_entry"
  window          <- windowNew
  set window [ windowTitle          := "афишка" ]
  minPriceEntry   <- builderGetObject builder castToEntry "min_price_entry"
  maxPriceEntry   <- builderGetObject builder castToEntry "max_price_entry"
  infoTextBuffer  <- builderGetObject builder castToTextBuffer "textbuffer_info"
  calendar        <- builderGetObject builder castToCalendar "calendar1"
--   priceL          <- builderGetObject builder castToLabel "label_price"
  oneBlock            <- builderGetObject builder castToBox "main_block1"
  mainBox         <- builderGetObject builder castToBox "box_view"

--   savs <- buttonNewWithLabel "Сохранить"
--   containerAdd window savs
  containerAdd window searchEntry
  containerAdd window searchButton

  savs `on` buttonActivated $ do
      saveRTF <- buttonNewWithLabel "Сохранить в RTF"
--       searchText <- entryGetText searchEntry
--       minPrice   <- entryGetText minPriceEntry
--       maxPrice   <- entryGetText maxPriceEntry
--       textBufferSetText infoTextBuffer searchText
--       containerRemove window vBox
--       (cYear, cMonth, cDay) <- calendarGetDate calendar
--       putStrLn searchText
      mainL <- labelNew $ Just "searchText"
      labelSetLabel mainL "Цена\n 800-900 р."
--       let minPr :: Int = if null minPrice then -1 else read minPrice
--       let maxPr :: Int = if null maxPrice then -1 else read maxPrice
--       containerAdd vBox saveRTF
      containerAdd window mainL

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


gui2 :: IO()
gui2 = do
  void initGUI
  window <- windowNew

  widgetShowAll window
  mainGUI


--   window <- windowNew
--   button <- buttonNew
--   set window [windowDefaultWidth := 800, windowDefaultHeight := 700,
--               containerChild := button, containerBorderWidth := 13]
--   onClicked button (hello button)
--   onDestroy window mainQuit
--   widgetShowAll window
--   mainGUI