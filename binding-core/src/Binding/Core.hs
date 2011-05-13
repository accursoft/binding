{-# LANGUAGE ExistentialQuantification #-}
module Binding.Core (module Binding.Variable, Bindable, bind, Source) where

import Binding.Variable

-- | A data binding:
-- @a@ is the type of the data source
-- @a -> d@ is a function that extracts data from the source
-- @t@ is the binding target
-- @d -> t -> IO ()@ is a function that applies data to the target
data Binding a = forall d t. Binding (a -> d) t (d -> t -> IO ())

-- | A simple binding source
data Source v a = Variable v => Source {bindings :: v [Binding a] -- ^ The source'a bindings
                                       ,var      :: v a}          -- ^ The bound variable

-- | Update a single binding.
update' :: a -> Binding a -> IO ()
update' source (Binding extract target apply) = apply (extract source) target

-- | Update a binding source's bindings.
update :: Source v a -> IO ()
update (Source bindings var) = do bindings <- readVar bindings
                                  a <- readVar var
                                  mapM_ (update' a) bindings

instance Variable v => Variable (Source v) where
    newVar a = do bindings <- newVar []
                  v <- newVar a
                  return $ Source bindings v

    readVar = readVar . var

    writeVar s a = writeVar (var s) a >> update s

    modifyVar s f = modifyVar (var s) f >> update s

    modifyVar' s f = do b <- modifyVar' (var s) f
                        update s
                        return b

-- | Binding sources
class Variable b => Bindable b where
    -- | Create a data binding
    bind :: b a               -- ^ The binding source
         -> (a -> d)          -- ^ A function that extracts data from the source
         -> t                 -- ^ The binding target
         -> (d -> t -> IO ()) -- ^ A function that applies data to the target
         -> IO ()

instance Variable v => Bindable (Source v) where
    bind (Source bindings var) extract target apply =
        do let binding = Binding extract target apply
           --update the new binding
           a <- readVar var
           update' a binding
           --add the new binding to the list
           modifyVar bindings (binding:)