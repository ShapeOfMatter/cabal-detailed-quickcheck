{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}

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

switchVerbosity :: Verbosity -> Bool -> Verbosity -> Verbosity
switchVerbosity v' q v = bool max min q v $ bool id pred q v'
-- ^ WARNING! This function does not work when passed Silent

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

useModifiers :: QC.Testable a => TestArgs -> a -> QC.Property
useModifiers TestArgs {verbosity, noShrinking, sizeScale} =
  foldr (.) QC.property $
    snd
      <$> filter
        fst
        [ (verbosity == Verbose, QC.verbose),
          (verbosity == VerboseShrinking, QC.verboseShrinking),
          (noShrinking, QC.noShrinking),
          (sizeScale /= 1, QC.mapSize (* sizeScale))
        ]

qcTestArgs :: QC.Testable a => TestArgs -> a -> IO QC.Result
qcTestArgs args property = QC.quickCheckWithResult (testArgsToArgs args) (useModifiers args property)

switchVIn :: Verbosity -> Bool -> TestArgs -> TestArgs
switchVIn v' q args@TestArgs {verbosity} = args {verbosity = switchVerbosity v' q verbosity}

setArgStr :: String -> String -> Maybe (TestArgs -> TestArgs)
setArgStr "silent" str =
  readMaybe str <&> \val args@TestArgs {verbosity} ->
    if val
      then args {verbosity = Silent}
      else args {verbosity = max Chatty verbosity}
setArgStr "chatty" str = readMaybe str <&> switchVIn Chatty
setArgStr "verbose" str = readMaybe str <&> switchVIn Verbose
setArgStr "verboseShrinking" str = readMaybe str <&> switchVIn VerboseShrinking
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

getOptionDescrs :: TestArgs -> [T.OptionDescr]
getOptionDescrs TestArgs {..} =
  [ T.OptionDescr
      { optionName = "silent",
        optionDescription = "Suppress QuickCheck output",
        optionType = T.OptionBool,
        optionDefault = Just . show $ verbosity == Silent
      },
    T.OptionDescr
      { optionName = "chatty",
        optionDescription = "Print QuickCheck output",
        optionType = T.OptionBool,
        optionDefault = Just . show $ verbosity > Chatty
      },
    T.OptionDescr
      { optionName = "verbose",
        optionDescription = "Print checked values",
        optionType = T.OptionBool,
        optionDefault = Just . show $ verbosity > Verbose
      },
    T.OptionDescr
      { optionName = "verboseShrinking",
        optionDescription = "Print all checked and shrunk values",
        optionType = T.OptionBool,
        optionDefault = Just . show $ verbosity == VerboseShrinking
      },
    T.OptionDescr
      { optionName = "verbosity",
        optionDescription = "Verbosity level",
        optionType = T.OptionEnum ["Silent", "Chatty", "Verbose", "VerboseShrinking"],
        optionDefault = Just $ show verbosity
      },
    T.OptionDescr
      { optionName = "maxDiscardRatio",
        optionDescription = "Maximum number of discarded tests per successful test before giving up",
        optionType = positiveIntType,
        optionDefault = Just $ show maxDiscardRatio
      },
    T.OptionDescr
      { optionName = "noShrinking",
        optionDescription = "Disable shrinking",
        optionType = T.OptionBool,
        optionDefault = Just $ show noShrinking
      },
    T.OptionDescr
      { optionName = "shrinking",
        optionDescription = "Enable shrinking",
        optionType = T.OptionBool,
        optionDefault = Just . show $ not noShrinking
      },
    T.OptionDescr
      { optionName = "maxShrinks",
        optionDescription = "Maximum number of shrinks to before giving up or zero to disable shrinking",
        optionType = positiveIntType,
        optionDefault = Just $ show maxShrinks
      },
    T.OptionDescr
      { optionName = "maxSuccess",
        optionDescription = "Maximum number of successful tests before succeeding",
        optionType = positiveIntType,
        optionDefault = Just $ show maxSuccess
      },
    T.OptionDescr
      { optionName = "maxSize",
        optionDescription = "Size to use for the biggest test cases",
        optionType = positiveIntType,
        optionDefault = Just $ show maxSize
      },
    T.OptionDescr
      { optionName = "sizeScale",
        optionDescription = "Scale all sizes by a number",
        optionType = positiveIntType,
        optionDefault = Just $ show sizeScale
      }
  ]

data PropertyTest prop = PropertyTest
  { name :: String,
    tags :: [String],
    property :: prop
  }

-- TODO: Figure out how to concisely offer variants of this function (drop some?)
getPropertyTestInstanceWithTestArgsUsingTestArgs ::
  QC.Testable prop =>
  TestArgs ->
  PropertyTest (TestArgs -> prop) ->
  T.TestInstance
getPropertyTestInstanceWithTestArgsUsingTestArgs originalArgs PropertyTest {..} =
  let withArgs args =
        T.TestInstance
          { run = do
              result <- qcTestArgs args (property args)
              return $ T.Finished case result of
                QC.Success {} -> T.Pass
                QC.GaveUp {} -> T.Error $ "GaveUp: QuickCheck gave up" ++ "\n" ++ show result
                QC.Failure {} -> T.Fail $ "Failure: A property failed" ++ "\n" ++ show result
                QC.NoExpectedFailure {} -> T.Fail $
                  "NoExpectedFailure: A property that should have failed did not" ++
                    "\n" ++ show result,
            name,
            tags,
            options = getOptionDescrs originalArgs,
            setOption = \opt str -> case setArgStr opt str of
              Nothing -> Left "Parse error"
              Just f -> Right . withArgs $ f args
          }
   in withArgs originalArgs
