{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildcards #-}

module Distribution.TestSuite.QuickCheck (
    Verbosity(..),
    TestArgs(..)
) where

import Test.QuickCheck as QC
import Text.Read (readMaybe)
import Data.Functor ((<&>))
import Data.Bool (bool)

data Verbosity = Silent | Chatty | Verbose | VerboseShrinking deriving (Eq, Ord, Show, Read, Enum, Bounded)

toggleVerbosity :: Verbosity -> Bool -> Verbosity -> Verbosity
toggleVerbosity Silent False _ = Chatty
toggleVerbosity v' q v = bool q max min v $ bool q id pred v'

data TestArgs = TestArgs
  { verbosity :: Verbosity,
    maxDiscardRatio :: Int,
    noShrinking :: Bool,
    maxShrinks :: Int,
    maxSuccess :: Int,
    maxSize :: Int,
    sizeScale :: Int
  }

argsToTestArgs :: Args -> TestArgs
argsToTestArgs Args{..} = TestArgs {
    verbosity = if chatty then Chatty else Silent,
    maxDiscardRatio,
    noShrinking = False,
    maxShrinks, maxSuccess, maxSize,
    sizeScale = 1 }

testArgsToArgs :: TestArgs -> Args
testArgsToArgs TestArgs{
    verbosity, maxDiscardRatio, maxShrinks,
    maxSuccess, maxSize } = Args {
    maxSuccess, maxDiscardRatio, maxSize,
    chatty = verbosity >= Chatty,
    maxShrinks }

getModifiers :: Testable t => TestArgs -> t -> Property
getModifiers TestArgs{verbosity, noShrinking, sizeScale} = foldr (.) id $ filter fst [
    (verbosity == Verbose, verbose),
    (verbosity == VerboseShrinking, verboseShrinking),
    (noShrinking, QC.noShrinking),
    (sizeScale != 1, scale (* sizeScale))]

toggleVIn :: Verbosity -> Bool -> TestArgs -> TestArgs
toggleVIn v' q args@TestArgs{verbosity} = args {toggleVerbosity v' q verbosity}

setArgStr :: String -> String -> Maybe (TestArgs -> TestArgs)
setArgStr "silent" str = readMaybe str <&> toggleVIn Silent
setArgStr "chatty" str = readMaybe str <&> toggleVIn Chatty
setArgStr "verbose" str = readMaybe str <&> toggleVIn Verbose
setArgStr "verboseShrinking" str = readMaybe str <&> toggleVIn VerboseShrinking
setArgStr "verbosity" str = readMaybe str <&> \val args ->
    args {verbosity = val}
setArgStr "maxDiscardRatio" str = readMaybe str <&> \val args ->
    args {maxDiscardRatio = val}
setArgStr "noShrinking" str = readMaybe str <&> \val args ->
    args {noShrinking = val ss}
setArgStr "shrinking" str = readMaybe str <&> \val args ->
    args {noShrinking = not val} -- Is this code not DRY enough?
setArgStr "maxShrinks" str = readMaybe str <&> \val args ->
    args {maxShrinks = val}
setArgStr "maxSuccess" str = readMaybe str <&> \val args ->
    args {maxSuccess = val}
setArgStr "maxSize" str = readMaybe str <&> \val args ->
    args {maxSize = val}
setArgStr "sizeScale" str = readMaybe str <&> \val args ->
    args {sizeScale = val}
setArgStr _ _ = Nothing

testArgDescrs :: [OptionDescr]
testArgDescrs = []
