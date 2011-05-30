import Data.IORef
import Graphics.UI.Gtk

import Binding.Core
import Binding.Gtk

main = do --create widgits
          initGUI
          text1 <- entryNew
          text2 <- entryNew
          --bind them
          source <- newVar 0 :: IO (Source IORef Double)
          bindTextEntry source text1
          bindTextEntry source text2
          --arrange the widgits
          hBox <- hBoxNew True 0
          boxPackStartDefaults hBox text1
          boxPackStartDefaults hBox text2
          --create the main window
          window <- windowNew
          set window [containerChild := hBox, windowTitle := "Data Binding with Gtk2Hs"]
          onDestroy window mainQuit
          --start the application
          widgetShowAll window
          mainGUI