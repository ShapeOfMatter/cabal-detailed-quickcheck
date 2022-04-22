{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- NoFieldSelectors is implemented in GHC 9.2.2, but HLS doesn’t support it
-- {-# LANGUAGE NoFieldSelectors #-}

module Distribution.TestSuite.QuickCheck
  ( Verbosity (..),
    TestArgs (..),
  )
where

import Data.Bool (bool)
import Data.Functor ((<&>))
import qualified Distribution.TestSuite as T
import qualified Test.QuickCheck as QC
import Text.Read (readMaybe)

data Verbosity = Silent | Chatty | Verbose | VerboseShrinking
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

toggleVerbosity :: Verbosity -> Bool -> Verbosity -> Verbosity
toggleVerbosity Silent False _ = Chatty
toggleVerbosity v' q v = bool max min q v $ bool id pred q v'

data TestArgs = TestArgs
  { verbosity :: Verbosity,
    maxDiscardRatio :: Int,
    noShrinking :: Bool,
    maxShrinks :: Int,
    maxSuccess :: Int,
    maxSize :: Int,
    sizeScale :: Int
  }

argsToTestArgs :: QC.Args -> TestArgs
argsToTestArgs QC.Args {..} =
  TestArgs
    { verbosity = if chatty then Chatty else Silent,
      maxDiscardRatio,
      noShrinking = False,
      maxShrinks,
      maxSuccess,
      maxSize,
      sizeScale = 1
    }

stdTestArgs :: TestArgs
stdTestArgs = argsToTestArgs QC.stdArgs

testArgsToArgs :: TestArgs -> QC.Args
testArgsToArgs
  TestArgs
    { verbosity,
      maxDiscardRatio,
      maxShrinks,
      maxSuccess,
      maxSize
    } =
    QC.Args
      { replay = Nothing,
        maxSuccess,
        maxDiscardRatio,
        maxSize,
        chatty = verbosity >= Chatty,
        maxShrinks
      }

getModifiers :: QC.Testable a => TestArgs -> a -> QC.Property
getModifiers TestArgs {verbosity, noShrinking, sizeScale} =
  foldr (.) QC.property $ snd <$> filter fst [ (verbosity == Verbose, QC.verbose),
    (verbosity == VerboseShrinking, QC.verboseShrinking),
    (noShrinking, QC.noShrinking),
    (sizeScale /= 1, QC.mapSize (* sizeScale))
  ]

toggleVIn :: Verbosity -> Bool -> TestArgs -> TestArgs
toggleVIn v' q args@TestArgs {verbosity} = args {verbosity = toggleVerbosity v' q verbosity}

setArgStr :: String -> String -> Maybe (TestArgs -> TestArgs)
setArgStr "silent" str = readMaybe str <&> toggleVIn Silent
setArgStr "chatty" str = readMaybe str <&> toggleVIn Chatty
setArgStr "verbose" str = readMaybe str <&> toggleVIn Verbose
setArgStr "verboseShrinking" str = readMaybe str <&> toggleVIn VerboseShrinking
setArgStr "verbosity" str =
  readMaybe str <&> \val args ->
    args {verbosity = val}
setArgStr "maxDiscardRatio" str =
  readMaybe str <&> \val args ->
    args {maxDiscardRatio = val}
setArgStr "noShrinking" str =
  readMaybe str <&> \val args ->
    args {noShrinking = val}
setArgStr "shrinking" str =
  readMaybe str <&> \val args ->
    args {noShrinking = not val} -- Is this code not DRY enough?
setArgStr "maxShrinks" str =
  readMaybe str <&> \val args ->
    args {maxShrinks = val}
setArgStr "maxSuccess" str =
  readMaybe str <&> \val args ->
    args {maxSuccess = val}
setArgStr "maxSize" str =
  readMaybe str <&> \val args ->
    args {maxSize = val}
setArgStr "sizeScale" str =
  readMaybe str <&> \val args ->
    args {sizeScale = val}
setArgStr _ _ = Nothing

positiveIntType :: T.OptionType
positiveIntType =
  T.OptionNumber
    { optionNumberIsInt = True,
      optionNumberBounds = (Just "1", Nothing)
    }

testArgDescrs :: [T.OptionDescr]
testArgDescrs =
  [ T.OptionDescr
      { optionName = "silent",
        optionDescription = "Suppress QuickCheck output",
        optionType = T.OptionBool,
        optionDefault = Just "False"
      },
    T.OptionDescr
      { optionName = "chatty",
        optionDescription = "Print QuickCheck output",
        optionType = T.OptionBool,
        optionDefault = Just "True"
      },
    T.OptionDescr
      { optionName = "verbose",
        optionDescription = "Print checked values",
        optionType = T.OptionBool,
        optionDefault = Just "False"
      },
    T.OptionDescr
      { optionName = "verboseShrinking",
        optionDescription = "Print all checked and shrunk values",
        optionType = T.OptionBool,
        optionDefault = Just "False"
      },
    T.OptionDescr
      { optionName = "verbosity",
        optionDescription = "Verbosity level",
        optionType = T.OptionEnum ["Silent", "Chatty", "Verbose", "VerboseShrinking"],
        optionDefault = Just "Chatty"
      },
    T.OptionDescr
      { optionName = "maxDiscardRatio",
        optionDescription = "Maximum number of discarded tests per successful test before giving up",
        optionType = positiveIntType,
        optionDefault = Just "10"
      },
    T.OptionDescr {
      optionName = "noShrinking",
      optionDescription = "Disable shrinking",
      optionType = T.OptionBool,
      optionDefault = Just "False"
    },
    T.OptionDescr {
      optionName = "shrinking",
      optionDescription = "Enable shrinking",
      optionType = T.OptionBool,
      optionDefault = Just "True"
    },
    T.OptionDescr {
      optionName = "maxShrinks",
      optionDescription = "Maximum number of shrinks to before giving up or zero to disable shrinking",
      optionType = positiveIntType,
      optionDefault = Just . show $ maxBound @Int
    },
    T.OptionDescr {
      optionName = "maxSuccess",
      optionDescription = "Maximum number of successful tests before succeeding",
      optionType = positiveIntType,
      optionDefault = Just "100"
    },
    T.OptionDescr {
      optionName = "maxSize",
      optionDescription = "Size to use for the biggest test cases",
      optionType = positiveIntType,
      optionDefault = Just "100"
    },
    T.OptionDescr {
      optionName = "sizeScale",
      optionDescription = "Scale all sizes by a number",
      optionType = positiveIntType,
      optionDefault = Just "1"
    }
  ]

data PropertyTest prop = PropertyTest {
  name :: String,
  tags :: [String],
  property :: prop
}

--getPropTestInsWTArgsUArgs :: QC.Testable t => PropertyTest t -> T.TestInstance
--getPropTestInsWTArgsUArgs 
