import Data.IORef
import Control.Monad
import Graphics.UI.Gtk
import System.Directory

import Data.Binding.List
import Graphics.UI.Gtk.Binding

data Person = Person {name::String, age::Int, active::Bool} deriving (Read, Show)

main = do --create an input file if none exists
          doesFileExist "in.txt" >>= (`unless` writeFile "in.txt"
		     "[Person {name = \"Joe\", age = 32, active = True},Person {name = \"Fred\", age = 50, active = False},Person {name = \"Alice\", age = 43, active = True}]")
          --read the input
          f <- readFile "in.txt"
          bl <- toBindingList $ read f :: IO (BindingList IORef Person)
          --create widgits
          initGUI
          name' <- entryNew
          age' <- spinButtonNewWithRange 0 120 1
          active' <- checkButtonNew
          --bind them
          nav <- navigation bl $ Person "" 0 False
          bindControl bl name name' entryText (\p n -> p {name = n})
          bindControl bl (fromIntegral . age) age' spinButtonValue (\p a -> p {age = round a})
          bindControl bl active active' toggleButtonActive (\p a -> p {active = a})
          --arrange the widgits
          table <- tableNew 3 2 True

          zipWithM_ (\cap row -> do label <- labelNew $ Just cap
                                    tableAttachDefaults table label 0 1 row (row+1))
                    ["Name:", "Age:", "Active:"] [0..2]

          zipWithM_ (\wid row -> tableAttachDefaults table wid 1 2 row (row+1))
                    [toWidget name', toWidget age', toWidget active'] [0..2]

          vBox <- vBoxNew False 0
          boxPackStartDefaults vBox table
          boxPackStartDefaults vBox nav
          --create the main window
          window <- windowNew
          set window [containerChild := vBox, windowTitle := "Data Binding with Gtk2Hs"]
          onDestroy window mainQuit
          --start the application
          widgetShowAll window
          mainGUI
          new <- fromBindingList bl
          writeFile "out.txt" $ show new