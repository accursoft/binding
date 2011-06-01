{-# LANGUAGE FlexibleContexts #-}
module Graphics.UI.Gtk.Binding where

import Control.Monad
import Control.Monad.Trans
import Graphics.UI.Gtk

import Data.Binding.List as B

-- | Bind a 'Source' to a control.
bindToControl :: Bindable b =>
                 b a      -- ^ the binding source
              -> (a -> d) -- ^ a function that extracts data from the source
              -> c        -- ^ the target control
              -> Attr c d -- ^ the attribute of the control to bind to
              -> IO ()
bindToControl source extract control attribute = bind source extract control (\c d -> set c [attribute := d])

-- | Bind from a control to a 'Source'.
-- The source is updated when the control loses focus.
bindFromControl :: (WidgetClass c, Bindable b) =>
                   c             -- ^ the control
                -> Attr c d      -- ^ the attribute of the control to bind from
                -> (a -> d -> a) -- ^ a function that applies data from the control to the source
                -> b a           -- ^ the binding source
                -> IO (ConnectId c)
bindFromControl control attribute apply source =
   control `on` focusOutEvent $ liftIO $ do d <- get control attribute
                                            a <- readVar source
                                            writeVar source (apply a d)
                                            return False

-- | Create a two-way data binding.
bindControl :: (WidgetClass c, Bindable b) =>
               b a           -- ^ the binding source
            -> (a -> d)      -- ^ a function that extracts data from the source
            -> c             -- ^ the control
            -> Attr c d      -- ^ the attribute of the control to bind to
            -> (a -> d -> a) -- ^ a function that applies data from the control to the source
            -> IO (ConnectId c)
bindControl source extract control attribute apply = do
   bindToControl source extract control attribute
   bindFromControl control attribute apply source

-- | Create a simple two-way data binding for a 'Textual' control.
bindTextEntry :: (Show a, Read a, EntryClass c, WidgetClass c, Bindable b) =>
                  b a -- ^ the binding source
               -> c   -- ^ the control
               -> IO (ConnectId c)
bindTextEntry source control = do
   bindToControl source show control entryText
   control `on` focusOutEvent $ liftIO $ do d <- get control entryText
                                            writeVar source (read d)
                                            return False

-- | Create a set of navigation buttons for a binding list.
navigation :: Variable v =>
              BindingList v a -- ^ the binding list
           -> a               -- ^ the default value for inserts
           -> IO HButtonBox
navigation bl new = do spin <- spinButtonNewWithRange 0 1 1
                       let setRange = B.length bl >>= spinButtonSetRange spin 0 . fromIntegral . pred
                       setRange
                       afterValueSpinned spin $ spinButtonGetValueAsInt spin >>= seek bl >> return ()
                       buttons <- forM [("<<", spinButtonSetValue spin 0)
                                       ,(">>", spinButtonSpin spin SpinEnd 0)
                                       ,("+", insert bl new >>= spinButtonSetValue spin . fromIntegral >> setRange)
                                       ,("-", B.remove bl >>= spinButtonSetValue spin . fromIntegral >> setRange)]
                                       $ \(l,c) -> do b <- buttonNewWithLabel l
                                                      on b buttonActivated c
                                                      return b

                       --disable the delete button when there's only one element
                       let del = last buttons
                       del `on` buttonActivated $ do l <- B.length bl
                                                     del `set` [widgetSensitive := l > 1]

                       --inserting enables the delete button
                       (buttons !! 2) `on` buttonActivated $ del `set` [widgetSensitive := True]

                       box <- hButtonBoxNew
                       containerAdd box spin
                       mapM_ (containerAdd box) buttons
                       return box