import Control.Monad
import Data.IORef
import Data.List
import Graphics.UI.WX
import System.Directory

import Data.Binding.List
import Graphics.UI.WX.Binding

data Person = Person {name::String, age::Int, active::Bool} deriving (Read, Show)

main = do --create an input file if none exists
          doesFileExist "in.txt" >>= (`unless` writeFile "in.txt"
		     "[Person {name = \"Joe\", age = 32, active = True},Person {name = \"Fred\", age = 50, active = False},Person {name = \"Alice\", age = 43, active = True}]")
          --read the input
          f <- readFile "in.txt"
          bl <- toBindingList $ read f :: IO (BindingList IORef Person)
          start $ do --create widgits
                     window <- frame [text := "Data Binding with WxHaskell"]
                     name' <- entry window []
                     age' <- spinCtrl window 0 120 []
                     active' <- checkBox window []
                     --bind them
                     nav <- navigation window bl $ Person "" 0 False
                     bindControl bl name name' text (\p n -> p {name = n})
                     bindControl bl (fromIntegral . age) age' selection (\p a -> p {age = a})
                     bindControl bl active active' checked (\p a -> p {active = a})
                     --arrange the widgits
                     let labels = map (floatRight . label) ["Name:", "Age:", "Active:"]
                     let widgets = map floatLeft [widget name', widget age', widget active']
                     --start the application
                     set window [layout := column 10 [grid 10 10 $ transpose [labels, widgets], nav]
                                ,on closing := fromBindingList bl >>= \l -> writeFile "out.txt" (show l) >> propagateEvent]