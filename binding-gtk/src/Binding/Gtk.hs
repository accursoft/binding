{-# LANGUAGE FlexibleContexts #-}
module Binding.Gtk where

import Control.Monad
import Control.Monad.Trans
import Graphics.UI.Gtk

import Binding.List as B

-- | Bind a 'Source' to a control.
bindToControl :: Bindable b =>
                 b a      -- ^ The binding source
              -> (a -> d) -- ^ A function that extracts data from the source
              -> c        -- ^ The target control
              -> Attr c d -- ^ The attribute of the control to bind to
              -> IO ()
bindToControl source extract control attribute = bind source extract control (\c d -> set c [attribute := d])

-- | Bind from a control to a 'Source'.
-- The source is updated when the control loses focus.
bindFromControl :: (WidgetClass c, Bindable b) =>
                   c             -- ^ The control
                -> Attr c d      -- ^ The attribute of the control to bind from
                -> (a -> d -> a) -- ^ A function that applies data from the control to the source
                -> b a           -- ^ The binding source
                -> IO (ConnectId c)
bindFromControl control attribute apply source =
    control `on` focusOutEvent $ liftIO $ do d <- get control attribute
                                             a <- readVar source
                                             writeVar source (apply a d)
                                             return False

-- | Create a two-way data binding.
bindControl :: (WidgetClass c, Bindable b) =>
               b a           -- ^ The binding source
            -> (a -> d)      -- ^ A function that extracts data from the source
            -> c             -- ^ The control
            -> Attr c d      -- ^ The attribute of the control to bind to
            -> (a -> d -> a) -- ^ A function that applies data from the control to the source
            -> IO (ConnectId c)
bindControl source extract control attribute apply = do
    bindToControl source extract control attribute
    bindFromControl control attribute apply source

-- | Create a simple two-way data binding for a 'Textual' control.
bindTextEntry :: (Show a, Read a, EntryClass c, WidgetClass c, Bindable b) =>
                  b a -- ^ The binding source
               -> c   -- ^ The control
               -> IO (ConnectId c)
bindTextEntry source control = do
    bindToControl source show control entryText
    control `on` focusOutEvent $ liftIO $ do d <- get control entryText
                                             writeVar source (read d)
                                             return False

-- | Create a set of navigation buttons for a binding list.
navigation :: Variable v =>
              BindingList v a -- ^ The binding list
           -> a               -- ^ The default value for inserts
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

                       let del = last buttons
                       on del buttonActivated $ do l <- B.length bl
                                                   set del [widgetSensitive := l > 1]

                       on (buttons !! 2) buttonActivated $ set del [widgetSensitive := True] --"+"

                       box <- hButtonBoxNew
                       containerAdd box spin
                       mapM_ (containerAdd box) buttons
                       return box