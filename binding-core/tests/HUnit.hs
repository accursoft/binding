{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TupleSections #-}

module HUnit where

import Test.Framework
import Test.HUnit.Lang

import Control.Monad
import Data.IORef
import System.Random

import Data.Binding.List as B
import Prelude as P

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Change these to exercise different variable and data types
type V = IORef
type A = Int

-- *** Test pure helpers ***

-- | Generate a list for testing.
-- Many operations are expected to fail on lists of less than 2 elements.
list' :: IO ([A], Int)
list' = do size <- randomRIO (2,100)
           list <- replicateM size randomIO
           return (list, size)

test_Remove' = do (list, size) <- list'
                  pos <- randomRIO (0, size-2)
                  let actual = remove' list pos
                  assertEqualVerbose "List hasn't shrunk correctly" (size-1) (P.length actual)
                  assertEqualVerbose "Head of list incorrect" (take pos list) (take pos actual)
                  assertEqualVerbose "Tail of list incorrect" (drop (pos+1) list) (drop pos actual)

test_RemoveLast' :: Assertion
test_RemoveLast' = do (list, size) <- list'
                      let actual = remove' list (size-1)
                      assertEqualVerbose "List hasn't shrunk correctly" (size-1) (P.length actual)
                      assertEqualVerbose "List is incorrect" (take (size-1) list) actual

test_Insert' :: Assertion
test_Insert' = do (list, size) <- list'
                  pos <- randomRIO (0, size-1)
                  new <- randomIO
                  let actual = insert' list pos new
                  assertEqualVerbose "List hasn't shrunk correctly" (size+1) (P.length actual)
                  assertEqualVerbose "Head of list incorrect" (take pos list) (take pos actual)
                  assertEqualVerbose "Element not inserted" new (actual !! pos)
                  assertEqualVerbose "Tail of list incorrect" (drop pos list) (drop (pos+1) actual)

--- *** Test monadic functions ***

test_Source :: Assertion
test_Source = do --bind a source
                 expected <- randomIO
                 source <- newVar expected :: IO (Source V A)
                 target <- randomIO >>= newVar :: IO (Source V A)
                 bind source id target writeVar
                 actual <- readVar target
                 assertEqualVerbose "Initial Bind" expected actual
                 --change its value
                 expected <- randomIO
                 writeVar source expected
                 actual <- readVar target
                 assertEqualVerbose "Value Changed" expected actual

-- | Generate a 'BindingList' for testing.
list :: IO ([A], Int, BindingList V A)
list = do (list, size) <- list'
          liftM (list, size,) (toBindingList list)

-- | Assert that a 'BindingList' holds the expected list.
assertList :: [A] -> BindingList V A -> Assertion
assertList list bl = fromBindingList bl >>= assertEqual list

-- | Assert that a 'BindingList' holds the expected list.
assertPos :: Int -> BindingList V A -> Int -> Assertion
assertPos expected bl reported = do pos <- position bl
                                    assertEqualVerbose "Wrong positon" expected pos
                                    assertEqualVerbose "Wrong positon reported" pos reported

test_List :: Assertion
test_List = do (expected, _, bl) <- list
               assertList expected bl

test_Length :: Assertion
test_Length = do (_, expected, bl) <- list
                 B.length bl >>= assertEqual expected

test_Seek :: Assertion
test_Seek = do (list, size, bl) <- list
               pos <- randomRIO (0,size-1)
               seek bl pos >>= assertPos pos bl
               actual <- readVar bl
               assertEqual (list !! pos) actual

test_SeekBy :: Assertion
test_SeekBy = do (_, size, bl) <- list
                 init <- randomRIO (0, size-1)
                 offset <- randomRIO (-init, size-init-1)
                 let expected = init + offset
                 seek bl init
                 actual <- seekBy (offset+) bl
                 --give a more detailed error message than assertPos
                 assertEqualVerbose ("Seek from " ++ show init ++ " by " ++ show offset) expected actual
                 assertPos expected bl actual

test_Next :: Assertion
test_Next = do (_, size, bl) <- list
               init <- randomRIO (0, size-2)
               seek bl init
               B.next bl >>= assertPos (init+1) bl

test_Prev :: Assertion
test_Prev = do (_, size, bl) <- list
               init <- randomRIO (1, size-1)
               seek bl init
               prev bl >>= assertPos (init-1) bl

test_Remove :: Assertion
test_Remove = do (list, size, bl) <- list
                 pos <- randomRIO (0, size-2)
                 seek bl pos
                 remove bl >>= assertPos pos bl
                 assertList (remove' list pos) bl

test_RemoveLast :: Assertion
test_RemoveLast = do (list, size, bl) <- list
                     seek bl (size-1)
                     remove bl >>= assertPos (size-2) bl
                     assertList (remove' list (size-1)) bl

test_Insert :: Assertion
test_Insert = do (list, size, bl) <- list
                 pos <- randomRIO (0, size-1)
                 new <- randomIO
                 seek bl pos
                 let pos' = pos+1
                 insert bl new >>= assertPos pos' bl
                 assertList (insert' list pos' new) bl