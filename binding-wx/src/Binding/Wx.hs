-- WxHaskell declares @type Var a = Tvar a@ as the standard type for mutable variables.
module Binding.Wx where

import Control.Monad
import Control.Concurrent.STM
import Graphics.UI.WX.Attributes
import Graphics.UI.WX.Classes
import Graphics.UI.WX.Events

import Binding.Core

-- | Bind a 'Source' to a control
bindToControl :: Source TVar a -- ^ The binding source
              -> (a -> d)      -- ^ A function that extracts data from the source
              -> c             -- ^ The target control
              -> Attr c d      -- ^ The attribute of the control to bind to
              -> IO ()
bindToControl source extract control attribute = bindSource source extract control (\d t -> set t [attribute := d])

-- | Bind from a control to a 'Source'
-- The source is updated when the control loses focus
bindFromControl :: Reactive c => c -- ^ The control
                -> Attr c d        -- ^ The attribute of the control to bind from
                -> (d -> a)        -- ^ A function that applies data to the source
                -> Source TVar a   -- ^ The binding source
                -> IO ()
bindFromControl control attribute apply source =
    set control [on focus := \f -> unless f $ do d <- get control attribute
                                                 writeSource source (apply d)
                                                 propagateEvent]

-- | Create a two-way data binding
bindControl :: Reactive c =>
               Source TVar a -- ^ The binding source
            -> (a -> d)      -- ^ A function that extracts data from the source
            -> c             -- ^ The control
            -> Attr c d      -- ^ The attribute of the control to bind to
            -> (d -> a)      -- ^ A function that applies data from the control to the source
            -> IO ()
bindControl source extract control attribute apply = do
    bindSource source extract control (\d c -> set c [attribute := d])
    set control [on focus := \f -> unless f $ do d <- get control attribute
                                                 writeSource source (apply d)
                                                 propagateEvent]

-- | Create a simple two-way data binding for a 'Textual' control
bindTextual :: (Show a, Read a, Textual c, Reactive c) =>
               Source TVar a -- ^ The binding source
            -> c             -- ^ The control
            -> IO ()
bindTextual source control = do
    bindSource source show control (\d c -> set c [text := d])
    set control [on focus := \f -> unless f $ do d <- get control text
                                                 writeSource source (read d)
                                                 propagateEvent]