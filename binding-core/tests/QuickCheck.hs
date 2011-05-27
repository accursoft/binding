{-# LANGUAGE TupleSections, TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Monadic
import Test.QuickCheck.All
import Test.QuickCheck.Test

import Control.Monad
import Data.IORef
import System.Exit

import Binding.List as B
import Prelude as P

-- Change these to exercise different variable and data types
type V = IORef
type A = Char

-- *** Helpers to generate random lists and positions ***

-- | A random list with at least two elements
newtype List = List [A] deriving Show

instance Arbitrary List where
    arbitrary = liftM List $ choose (2, 100) >>= vector
    shrink (List xs) = [List ys | ys <- shrink xs, P.length ys > 1]

-- | Maps @i@ to a position in @xs@
anywhere :: Int -> [A] -> Int
anywhere i xs = let max = P.length xs - 1
                in if max == 0 then 0 else i `mod` max

-- | Anywhere in the list except the last element
notLast :: Int -> [A] -> Int
notLast i = anywhere i . tail

-- | Create a 'BindingList', and 'seek' to @pos@
list :: [A] -> Int -> IO (BindingList V A)
list xs pos = do bl <- toBindingList xs
                 seek bl pos
                 return bl

-- *** Test pure functions ***

prop_remove' :: [A] -> Int -> Bool
prop_remove' xs i = let pos = anywhere i xs
                        actual = remove' xs pos
                    in P.length actual == P.length xs - 1
                    && take pos actual == take pos xs
                    && drop (pos+1) xs == drop pos actual

prop_removeLast' :: [A] -> Bool
prop_removeLast' xs = let pos = P.length xs - 1
                          actual = remove' xs pos
                      in P.length actual == pos
                      && actual == take pos xs

prop_insert' :: [A] -> Int -> A -> Bool
prop_insert' xs i x = let pos = anywhere i xs
                          actual = insert' xs pos x
                      in P.length actual == P.length xs + 1
                      && take pos actual == take pos xs
                      && actual !! pos == x
                      && drop pos actual == drop (pos+1) xs

-- *** QuickCheck 'Property's for Monadic actions. ***

prop_Source :: (A,A,A) -> Property
prop_Source (a,b,c) = monadicIO $ do
    (x,y) <- run $ do --bind a source
                   source <- newVar a :: IO (Source V A)
                   target <- newVar c :: IO (Source V A)
                   bind source id target writeVar
                   x <- readVar target
                   --change its value
                   writeVar source b
                   y <- readVar target
                   return (x,y)
    assert (x==a && y==b)

prop_List :: NonEmptyList A -> Property
prop_List (NonEmpty xs) = monadicIO $ do
    ys <- run $ (toBindingList xs :: IO (BindingList V A)) >>= fromBindingList
    assert (ys == xs)

prop_length :: NonEmptyList A -> Property
prop_length (NonEmpty xs) = monadicIO $ do
    l <- run $ (toBindingList xs :: IO (BindingList V A)) >>= B.length
    assert (l == P.length xs)

prop_seek :: NonEmptyList A -> Int -> Property
prop_seek (NonEmpty xs) i = let pos = anywhere i xs in monadicIO $ do
    (new, x) <- run $ do bl <- toBindingList xs :: IO (BindingList V A)
                         liftM2 (,) (seek bl pos) (readVar bl)
    assert (new == pos && x == xs !! pos)

prop_position :: NonEmptyList A -> Int -> Property
prop_position (NonEmpty xs) i = let pos = anywhere i xs in monadicIO $ do
    new <- run $ list xs pos >>= position
    assert (new == pos)

prop_seekBy :: List -> Int -> Int -> Property
prop_seekBy (List xs) a b = let size = P.length xs
                                init = anywhere a xs
                                offset = anywhere b xs - init
                            in monadicIO $ do
    (new, x) <- run $ do bl <- list xs init
                         liftM2 (,) (seekBy (offset+) bl) (readVar bl)
    assert (new == init + offset && x == xs !! new)

prop_next :: List -> Int -> Property
prop_next (List xs) i = let pos = notLast i xs in monadicIO $ do
    (new, x) <- run $ do bl <- list xs pos
                         liftM2 (,) (B.next bl) (readVar bl)
    assert (new == pos + 1 && x == xs !! new)

prop_prev :: List -> Int -> Property
prop_prev (List xs) i = let pos = anywhere i xs + 1 in monadicIO $ do
    (new, x) <- run $ do bl <- list xs pos
                         liftM2 (,) (prev bl) (readVar bl)
    assert (new == pos - 1 && x == xs !! new)

prop_insert :: List -> Int -> A -> Property
prop_insert (List xs) i x = let pos = anywhere i xs
                                new = pos + 1
                            in monadicIO $ do
    (pos', ys) <- run $ do bl <- list xs pos
                           liftM2 (,) (insert bl x) (fromBindingList bl)
    assert (ys == insert' xs new x && pos' == new)

-- we test removing the last element separately because it's a special case
testRemove :: [A] -> Int -> PropertyM IO (Int, [A])
testRemove xs pos = run $ do bl <- list xs pos
                             liftM2 (,) (remove bl) (fromBindingList bl)

prop_remove :: List -> Int -> Property
prop_remove (List xs) i = let pos = notLast i xs in monadicIO $ do
    (pos', ys) <- testRemove xs pos
    assert (ys == remove' xs pos && pos' == pos)

prop_removeLast :: List -> Property
prop_removeLast (List xs) = let pos = P.length xs - 1 in monadicIO $ do
    (pos', ys) <- testRemove xs pos
    assert (ys == remove' xs pos && pos' == pos -1)

-- | Test the 'Property's
main = do passed <- $quickCheckAll
          unless passed exitFailure