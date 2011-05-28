{-# LANGUAGE RankNTypes #-}
module Binding.Wx where

import Control.Monad
import Graphics.UI.WX

import Binding.List as B

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
bindFromControl :: (Bindable b, Reactive c) =>
                   c             -- ^ the control
                -> Attr c d      -- ^ the attribute of the control to bind from
                -> (a -> d -> a) -- ^ a function that applies data from the control to the source
                -> b a           -- ^ the binding source
                -> IO ()
bindFromControl control attribute apply source =
   set control [on focus := \f -> unless f $ do d <- get control attribute
                                                a <- readVar source
                                                writeVar source (apply a d)
                                                propagateEvent]

-- | Create a two-way data binding.
bindControl :: (Bindable b, Reactive c) =>
               b a           -- ^ the binding source
            -> (a -> d)      -- ^ a function that extracts data from the source
            -> c             -- ^ the control
            -> Attr c d      -- ^ the attribute of the control to bind to
            -> (a -> d -> a) -- ^ a function that applies data from the control to the source
            -> IO ()
bindControl source extract control attribute apply = do
   bindToControl source extract control attribute
   bindFromControl control attribute apply source

-- | Create a simple two-way data binding for a 'Textual' control.
bindTextual :: (Show a, Read a, Bindable b, Textual c, Reactive c) =>
               b a -- ^ the binding source
            -> c   -- ^ the control
            -> IO ()
bindTextual source control = do
   bindToControl source show control text
   set control [on focus := \f -> unless f $ do d <- get control text
                                                writeVar source (read d)
                                                propagateEvent]

-- | Create a set of navigation buttons for a binding list.
navigation :: Variable v =>
              Window w        -- ^ the buttons' owner
           -> BindingList v a -- ^ the binding list
           -> a               -- ^ the default value for inserts
           -> IO Layout
navigation owner bl new = do l <- B.length bl
                             spin <- spinCtrl owner 0 (l-1) [on select ::= \s -> get s selection >>= seek bl >> return ()]
                             let go i = set spin [selection := i] >> seek bl i
                             buttons <- forM [("<<", go 0)
                                             ,(">>", B.length bl >>= go . pred)
                                             ,("+", insert bl new >>= go)
                                             ,("-", remove bl >>= go)]
                                             $ \(t,c) -> button owner [text := t, on command := c >> return ()]

                             let del = last buttons
                             set del [on command :~ (>> do l <- B.length bl
                                                           set del [enabled := l > 1])                                                               ]

                             set (buttons !! 2) [on command :~ (>> set del [enabled := True])] --"+"

                             return $ row 0 $ widget spin : map widget buttons