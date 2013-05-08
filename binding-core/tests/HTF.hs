{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import {-@ HTF_TESTS @-} HUnit
import {-@ HTF_TESTS @-} QuickCheck

main = htfMain htf_importedTests