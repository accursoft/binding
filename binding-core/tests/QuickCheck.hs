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

-- | A random list with at least two elements
newtype List = List [A] deriving Show

instance Arbitrary List where
    arbitrary = liftM List $ choose (2, 100) >>= vector
    shrink (List xs) = [List ys | ys <- shrink xs, P.length ys > 1]

-- | Maps @x@ to the range [0..@max@]
clamp :: Int -> Int -> Int
clamp x max = if max == 0 then 0 else x `mod` max

-- | Maps @i@ to a position in @xs@, up to @1 - o@ from the end
pos' :: Int -> Int -> [A] -> Int
pos' o i xs = clamp i (P.length xs - o)

-- | Anywhere in the list
anywhere = pos' 1

-- | Anywhere in the list except the last element
notLast = pos' 2

-- | Create a 'BindingList', and 'seek' to @pos@
list :: [A] -> Int -> IO (BindingList V A)
list xs pos = do bl <- toBindingList xs
                 seek bl pos
                 return bl

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