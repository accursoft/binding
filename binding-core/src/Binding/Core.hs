{-# LANGUAGE ExistentialQuantification #-}
module Binding.Core (Source, newSource, bindSource, readSource, writeSource, modifySource) where

import Binding.Variable

-- | A data binding
-- @a@ is the type of the data source
-- @a -> d@ is a function that extracts data from the source
-- @t@ is the binding target
-- @d -> t -> IO ()@ is a function that applies data to the target
data Binding a = forall d t. Binding (a -> d) t (d -> t -> IO ())

-- | A binding source
data Source v a = Variable v => Source {bindings::v [Binding a] -- ^ The source's bindings
                                       ,source::v a}            -- ^ The data source

-- | Update a single binding
update' :: a -> Binding a -> IO ()
update' source (Binding extract target apply) = apply (extract source) target

-- | Update a binding source's bindings
update :: Source v a -> IO ()
update (Source bindings source) = do bindings <- readVar bindings
                                     source <- readVar source
                                     mapM_ (update' source) bindings

-- | Create a binding source
newSource :: Variable v => a -> IO (Source v a)
newSource a = do source <- newVar a
                 bindings <- newVar []
                 return $ Source bindings source

-- | Create a data binding
bindSource :: Source v a        -- ^ The binding source
           -> (a -> d)          -- ^ A function that extracts data from the source
           -> t                 -- ^ The binding target
           -> (d -> t -> IO ()) -- ^ A function that applies data to the target
           -> IO ()
bindSource (Source bindings source) extract target apply =
    do let binding = Binding extract target apply
       --activate the new binding
       source <- readVar source
       update' source binding
       --add the new binding to the list
       modifyVar bindings (binding:)

-- | Read a binding source
readSource :: Variable v => Source v a -> IO a
readSource a = readVar (source a)

-- | Write a binding source's data
writeSource :: Variable v => Source v a -> a -> IO ()
writeSource a d = writeVar (source a) d >> update a

-- | Modify a binding source's data
modifySource :: Variable v => Source v a -> (a -> a) -> IO ()
modifySource a f = modifyVar (source a) f >> update a

-- | Modify a binding source's data
modifySource' :: Variable v => Source v a -> (a -> (a,b)) -> IO b
modifySource' a f = do b <- modifyVar' (source a) f
                       update a
                       return b