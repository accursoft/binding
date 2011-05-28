{-# LANGUAGE ExistentialQuantification #-}
module Binding.List (module Binding.Core, BindingList, toBindingList, fromBindingList, length, position, seek, seekBy, next, prev, remove', remove, insert', insert) where

import Prelude hiding (length)
import qualified Prelude as P
import Control.Monad

import Binding.Core

-- | Associates a binding source with a list of data sources.
data BindingList v a = Variable v => BindingList {source :: Source v a -- ^ the list's binding source
                                                 ,list   :: v [v a]    -- ^ the bound list
                                                 ,pos    :: v Int}     -- ^ the current position
-- [v a] is itself in a Variable, to allow for insertions and deletions.

-- | Create a binding list.
toBindingList :: Variable v => [a] -> IO (BindingList v a)
toBindingList [] = error "empty list"
toBindingList list = do list'<- mapM newVar list >>= newVar
                        source <- newVar (head list)
                        pos <- newVar 0
                        return $ BindingList source list' pos

-- | Update the binding list from the 'source'.
update :: BindingList v a -> IO ()
update (BindingList source list pos) = do list' <- readVar list
                                          pos' <- readVar pos
                                          readVar source >>= writeVar (list' !! pos')

-- | Extract the data from a binding list.
fromBindingList :: Variable v => BindingList v a -> IO [a]
fromBindingList b = do update b
                       readVar (list b) >>= mapM readVar

-- | interface to the binding list's 'Source'
instance Variable v => Variable (BindingList v) where
   {- WARNING warn "Did you mean to use newBindingList?" -}
   newVar = warn where warn a = toBindingList [a]
   readVar = readVar . source
   writeVar = writeVar . source
   modifyVar = modifyVar . source
   modifyVar' = modifyVar' . source

instance Variable v => Bindable (BindingList v) where
   bind = bind . source

-- | The size of a binding list.
length :: Variable v => BindingList v a -> IO Int
length b = do list <- readVar (list b)
              return $ P.length list

-- | Get the current position.
position :: Variable v => BindingList v a -> IO Int
position b = readVar $ pos b

-- | Bind to a new position in a binding list.
-- Returns the new position; this is convenient for 'seekBy' and friends.
seek:: Variable v => BindingList v a -> Int -> IO Int
seek b new = do pos' <- readVar $ pos b
                if pos' == new then return new else update b >> seek' b new

-- | Unconditional seek. Called after elements have changed position.
seek':: BindingList v a -> Int -> IO Int
seek' (BindingList source list pos) new = do list' <- readVar list
                                             readVar (list' !! new) >>= writeVar source
                                             writeVar pos new
                                             return new

-- | Bind to a new position in a binding list.
seekBy :: Variable v => (Int -> Int) -> BindingList v a -> IO Int
seekBy f bindingList = do pos <- readVar (pos bindingList)
                          seek bindingList $ f pos

-- | Bind to the next item in a binding list.
next :: Variable v => BindingList v a -> IO Int
next = seekBy succ

-- | Bind to the previous item in a binding list.
prev :: Variable v => BindingList v a -> IO Int
prev = seekBy pred

-- | Remove an element from a list.
remove' :: [a] -> Int ->  [a]
remove' list pos = let (xs, _:ys) = splitAt pos list
                   in xs ++ ys

-- | Remove the current element from the list.
remove :: BindingList v a -> IO Int
remove b@(BindingList _ list pos) = do list' <- readVar list
                                       pos' <- readVar pos
                                       writeVar list $ remove' list' pos'
                                       seek' b (if pos' == P.length list' - 1 then pos' - 1 else pos')

-- | Insert an element into a list.
insert' :: [a] -> Int -> a -> [a]
insert' list pos x = let (xs, ys) = splitAt pos list
                     in xs ++ [x] ++ ys

-- | Insert an element into the list.
-- The new element is inserted after the current element.
-- This allows appending, but precludes prepending.
insert :: BindingList v a -> a -> IO Int
insert b@(BindingList _ list pos) x = do update b
                                         list' <- readVar list
                                         pos' <- readVar pos
                                         x' <- newVar x
                                         let pos'' = pos' + 1
                                         writeVar list $ insert' list' pos'' x'
                                         seek' b pos''