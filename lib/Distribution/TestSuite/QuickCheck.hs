{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Distribution.TestSuite.QuickCheck where

import Test.QuickCheck
import Text.Read
import Data.Functor
import Data.Bool

data Verbosity = Silent | Chatty | Verbose | VerboseShrinking deriving (Eq, Ord, Show, Read)

data TestArgs = TestArgs
  { chatty :: Bool,
    verbose :: Bool,
    verbosity :: Verbosity,
    maxDiscardRatio :: Int,
    noShrinking :: Bool,
    maxShrinks :: Int,
    maxSuccess :: Int,
    maxSize :: Int,
    mapSize :: Int
  }

argsToTestArgs :: Args -> TestArgs
argsToTestArgs Args {maxSuccess, maxDiscardRatio, maxSize, chatty, maxShrinks} = TestArgs {
    chatty,
    verbose = False,
    verbosity = if chatty then Chatty else Silent,
    maxDiscardRatio,
    noShrinking = False,
    maxShrinks, maxSuccess, maxSize,
    mapSize = 1
}

setArgStr :: String -> String -> Maybe (TestArgs -> TestArgs)
setArgStr "chatty" str = readMaybe str <&> \val args ->
    setVerbosity (bool max min val (verbosity args) Chatty) args
setArgStr _ _ = Nothing

setVerbosity = undefined
