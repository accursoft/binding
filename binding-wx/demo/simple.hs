import Data.IORef
import Graphics.UI.WX

import Binding.Core
import Binding.Wx

main = start $ do --create widgits
                  window <- frame [text := "Data Binding with WxHaskell"]
                  text1 <- entry window []
                  text2 <- entry window []
                  --bind them
                  source <- newVar 0 :: IO (Source IORef Double)
                  bindTextual source text1
                  bindTextual source text2
                  --start the application
                  set window [layout := row 0 [widget text1, widget text2]]