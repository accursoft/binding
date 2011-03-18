module Binding.Gtk where

import Control.Monad
import Control.Monad.Trans
import Graphics.UI.Gtk

import Binding.Variable
import Binding.Core

-- | Bind a 'Source' to a control
bindToControl :: Source v a -- ^ The binding source
              -> (a -> d)   -- ^ A function that extracts data from the source
              -> c          -- ^ The target control
              -> Attr c d   -- ^ The attribute of the control to bind to
              -> IO ()
bindToControl source extract control attribute = bindSource source extract control (\d t -> set t [attribute := d])

-- | Bind from a control to a 'Source'
-- The source is updated when the control loses focus
bindFromControl :: (WidgetClass c, Variable v) =>
                   c          -- ^ The control
                -> Attr c d   -- ^ The attribute of the control to bind from
                -> (d -> a)   -- ^ A function that applies data to the source
                -> Source v a -- ^ The binding source
                -> IO (ConnectId c)
bindFromControl control attribute apply source =
    control `on` focusOutEvent $ liftIO $ do t <- get control attribute
                                             writeSource source (apply t)
                                             return False

-- | Create a two-way data binding
bindControl :: (WidgetClass c, Variable v) =>
               Source v a -- ^ The binding source
            -> (a -> d)   -- ^ A function that extracts data from the source
            -> c          -- ^ The control
            -> Attr c d   -- ^ The attribute of the control to bind to
            -> (d -> a)   -- ^ A function that applies data from the control to the source
            -> IO (ConnectId c)
bindControl source extract control attribute apply = do
    bindSource source extract control (\d t -> set t [attribute := d])
    control `on` focusOutEvent $ liftIO $ do t <- get control attribute
                                             writeSource source (apply t)
                                             return False

-- | Create a simple two-way data binding for a 'Textual' control
bindTextEntry :: (Show a, Read a, EntryClass c, WidgetClass c, Variable v) =>
                  Source v a -- ^ The binding source
               -> c          -- ^ The control
               -> IO (ConnectId c)
bindTextEntry source control = do
    bindSource source show control (\d t -> set t [entryText := d])
    control `on` focusOutEvent $ liftIO $ do t <- get control entryText
                                             writeSource source (read t)
                                             return False