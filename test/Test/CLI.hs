{-# LANGUAGE OverloadedStrings #-}

module Test.CLI (tests) where

import Agda.Interaction.Library (parseLibName)
import Agda.Interaction.Options (CommandLineOptions (..), defaultOptions)
import Data.Aeson qualified as Aeson
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import AgdaMCP.CLI (
  Exit (..),
  Lifecycle (..),
  Mode (..),
  Outcome (..),
  Stream (..),
  answered,
  defaultLifecycle,
  helped,
  parseCommandLine,
  plumbing,
  rejected,
 )
import AgdaMCP.Daemon.Protocol (Response (..))
import AgdaMCP.Tools.Options (
  AgdaFlags (..),
  defaultFlags,
  renderFlags,
  toCommandLineOptions,
 )

tests :: TestTree
tests =
  testGroup
    "CLI"
    [ verbTests
    , optionTests
    , globalTests
    , helpTests
    , flagsTests
    , exitTests
    ]

-- Verbs

verbTests :: TestTree
verbTests =
  testGroup
    "verbs"
    [ testCase "no verb runs the MCP server" $
        parse [] @?= Parsed defaultFlags defaultLifecycle Serve
    , testCase "load sends the path made absolute against the working directory" $
        parse ["load", "src/X.agda"]
          @?= invoke "load" [("path", Aeson.String "/work/src/X.agda")]
    , testCase "load leaves an absolute path alone" $
        parse ["load", "/elsewhere/X.agda"]
          @?= invoke "load" [("path", Aeson.String "/elsewhere/X.agda")]
    , testCase "goal sends the load id and the goal" $
        parse ["goal", "L1-k7x2", "0"]
          @?= invoke "goal" [("load_id", loadId), ("goal", Aeson.Number 0)]
    , testCase "a leading question mark on the goal is stripped" $
        parse ["goal", "L1-k7x2", "?3"]
          @?= invoke "goal" [("load_id", loadId), ("goal", Aeson.Number 3)]
    , testCase "check sends the expression as well" $
        parse ["check", "L1-k7x2", "0", "suc zero"]
          @?= invoke
            "check"
            [ ("load_id", loadId)
            , ("goal", Aeson.Number 0)
            , ("expression", Aeson.String "suc zero")
            ]
    , testCase "infer sends the same shape as check" $
        parse ["infer", "L1-k7x2", "0", "suc zero"]
          @?= invoke
            "infer"
            [ ("load_id", loadId)
            , ("goal", Aeson.Number 0)
            , ("expression", Aeson.String "suc zero")
            ]
    , testCase "give sends a single-item batch with no action" $
        parse ["give", "L1-k7x2", "0", "refl"]
          @?= invoke
            "give"
            [ ("load_id", loadId)
            , ("gives", Aeson.toJSON [giveItem 0 "refl" Nothing])
            ]
    , testCase "give --refine sends the refine action" $
        parse ["give", "L1-k7x2", "0", "refl", "--refine"]
          @?= invoke
            "give"
            [ ("load_id", loadId)
            , ("gives", Aeson.toJSON [giveItem 0 "refl" (Just "refine")])
            ]
    , testCase "case-split with no variables sends an empty list" $
        parse ["case-split", "L1-k7x2", "0"]
          @?= invoke
            "case_split"
            [ ("load_id", loadId)
            , ("goal", Aeson.Number 0)
            , ("variables", Aeson.toJSON ([] :: [Text]))
            ]
    , testCase "case-split sends its variables in order" $
        parse ["case-split", "L1-k7x2", "0", "x", "y", "z"]
          @?= invoke
            "case_split"
            [ ("load_id", loadId)
            , ("goal", Aeson.Number 0)
            , ("variables", Aeson.toJSON ["x", "y", "z" :: Text])
            ]
    , testCase "daemon" $
        parse ["daemon"] @?= Parsed defaultFlags defaultLifecycle RunDaemon
    , testCase "stop" $
        parse ["stop"] @?= Parsed defaultFlags defaultLifecycle Stop
    , testCase "status" $
        parse ["status"] @?= Parsed defaultFlags defaultLifecycle Status
    , testCase "an unknown verb is rejected" $
        expectRejected $
          parse ["frobnicate"]
    , testCase "a missing expression is rejected" $
        expectRejected $
          parse ["check", "L1-k7x2", "0"]
    , testCase "a goal that is not an integer is rejected" $
        expectRejected $
          parse ["goal", "L1-k7x2", "zero"]
    , testCase "a missing goal is rejected" $
        expectRejected $
          parse ["goal", "L1-k7x2"]
    ]

-- Per-verb options

optionTests :: TestTree
optionTests =
  testGroup
    "options"
    [ testCase "normalization is sent only when given" $
        parse ["goal", "L1-k7x2", "0", "--normalization", "headnormal"]
          @?= invoke
            "goal"
            [ ("load_id", loadId)
            , ("goal", Aeson.Number 0)
            , ("normalization", Aeson.String "headnormal")
            ]
    , testCase "an unknown normalization is rejected" $
        expectRejected $
          parse ["goal", "L1-k7x2", "0", "--normalization", "very"]
    , testCase "timeout is sent in seconds only when given" $
        parse ["load", "/x/X.agda", "--timeout", "5"]
          @?= invoke
            "load"
            [("path", Aeson.String "/x/X.agda"), ("timeout", Aeson.Number 5)]
    , testCase "a timeout that is not an integer is rejected" $
        expectRejected $
          parse ["load", "/x/X.agda", "--timeout", "soon"]
    , testCase "an expression beginning with a dash follows a double dash" $
        parse ["check", "L1-k7x2", "0", "--", "-x"]
          @?= invoke
            "check"
            [ ("load_id", loadId)
            , ("goal", Aeson.Number 0)
            , ("expression", Aeson.String "-x")
            ]
    ]

-- Global flags before the verb

globalTests :: TestTree
globalTests =
  testGroup
    "global flags"
    [ testCase "Agda flags before the verb reach the flags record" $
        parse
          [ "-i"
          , "src"
          , "-l"
          , "standard-library"
          , "--no-default-libraries"
          , "load"
          , "/x/X.agda"
          ]
          @?= Parsed
            defaultFlags
              { includes = ["src"]
              , libraries = ["standard-library"]
              , defaultLibs = False
              }
            defaultLifecycle
            (Invoke "load" (Map.fromList [("path", Aeson.String "/x/X.agda")]))
    , testCase "Agda flags with no verb still run the MCP server" $
        parse ["--library-file", "/x/libraries", "--no-libraries"]
          @?= Parsed
            defaultFlags {libraryFile = Just "/x/libraries", useLibs = False}
            defaultLifecycle
            Serve
    , testCase "idle timeout in minutes" $
        parse ["--idle-timeout", "90m", "status"]
          @?= Parsed defaultFlags (Lifecycle 5400) Status
    , testCase "idle timeout in seconds" $
        parse ["--idle-timeout", "10s", "daemon"]
          @?= Parsed defaultFlags (Lifecycle 10) RunDaemon
    , testCase "idle timeout in hours" $
        parse ["--idle-timeout", "3h", "daemon"]
          @?= Parsed defaultFlags (Lifecycle 10800) RunDaemon
    , testCase "the default idle timeout is two hours" $
        defaultLifecycle @?= Lifecycle 7200
    , testCase "an idle timeout without a unit is rejected" $
        expectRejected $
          parse ["--idle-timeout", "90", "status"]
    , testCase "Agda flags after the verb are rejected" $
        expectRejected $
          parse ["status", "-i", "src"]
    ]

-- Help

helpTests :: TestTree
helpTests =
  testGroup
    "help"
    [ testCase "the top-level help says the bare command is the MCP server" $
        case parse ["--help"] of
          Helped text -> assertContains "MCP server" text
          other -> assertFailure $ "expected help, got " <> show other
    , testCase "the top-level help lists every verb" $
        case parse ["--help"] of
          Helped text ->
            mapM_
              (`assertContains` text)
              [ "load"
              , "goal"
              , "check"
              , "infer"
              , "give"
              , "case-split"
              , "daemon"
              , "stop"
              , "status"
              ]
          other -> assertFailure $ "expected help, got " <> show other
    , testCase "give's help says interrupting the client does not cancel the edit" $
        case parse ["give", "--help"] of
          Helped text -> assertContains "does not cancel" text
          other -> assertFailure $ "expected help, got " <> show other
    , testCase "case-split's help says the same" $
        case parse ["case-split", "--help"] of
          Helped text -> assertContains "does not cancel" text
          other -> assertFailure $ "expected help, got " <> show other
    , testCase "check's help mentions the double dash" $
        case parse ["check", "--help"] of
          Helped text -> assertContains "--" text
          other -> assertFailure $ "expected help, got " <> show other
    ]

-- AgdaFlags

flagsTests :: TestTree
flagsTests =
  testGroup
    "AgdaFlags"
    [ testCase "the defaults are Agda's defaults" $ do
        let options = toCommandLineOptions defaultFlags
        optIncludePaths options @?= optIncludePaths defaultOptions
        optLibraries options @?= optLibraries defaultOptions
        optOverrideLibrariesFile options @?= optOverrideLibrariesFile defaultOptions
        optUseLibs options @?= optUseLibs defaultOptions
        optDefaultLibs options @?= optDefaultLibs defaultOptions
    , testCase "include paths are prepended, as the fold always did" $
        optIncludePaths (toCommandLineOptions defaultFlags {includes = ["a", "b"]})
          @?= ["b", "a"]
    , testCase "libraries are appended in order and parsed" $
        optLibraries
          (toCommandLineOptions defaultFlags {libraries = ["standard-library", "cubical"]})
          @?= [parseLibName "standard-library", parseLibName "cubical"]
    , testCase "a library file overrides the default and turns libraries on" $ do
        let options =
              toCommandLineOptions
                defaultFlags {libraryFile = Just "/x/libraries"}
        optOverrideLibrariesFile options @?= Just "/x/libraries"
        optUseLibs options @?= True
    , testCase "no-libraries turns libraries off" $
        optUseLibs (toCommandLineOptions defaultFlags {useLibs = False}) @?= False
    , testCase "no-default-libraries turns the defaults off" $
        optDefaultLibs (toCommandLineOptions defaultFlags {defaultLibs = False})
          @?= False
    , testCase "rendering gives argv in canonical order with long spellings" $
        renderFlags
          AgdaFlags
            { includes = ["src", "lib"]
            , libraries = ["standard-library", "cubical"]
            , libraryFile = Just "/x/libraries"
            , useLibs = False
            , defaultLibs = False
            }
          @?= [ "--include-path"
              , "src"
              , "--include-path"
              , "lib"
              , "--library"
              , "standard-library"
              , "--library"
              , "cubical"
              , "--library-file"
              , "/x/libraries"
              , "--no-libraries"
              , "--no-default-libraries"
              ]
    , testCase "the defaults render as nothing" $
        renderFlags defaultFlags @?= []
    , testCase "rendering inverts parsing" $
        parse (renderFlags flags <> ["status"])
          @?= Parsed flags defaultLifecycle Status
    ]
 where
  flags =
    AgdaFlags
      { includes = ["src"]
      , libraries = ["standard-library"]
      , libraryFile = Just "/x/libraries"
      , useLibs = True
      , defaultLibs = False
      }

-- Exit mapping

exitTests :: TestTree
exitTests =
  testGroup
    "exit"
    [ testCase "a successful answer goes to stdout with exit 0" $
        answered (Response True "Loaded.")
          @?= Exit "Loaded." Stdout ExitSuccess
    , testCase "a refused answer goes to stderr with exit 1" $
        answered (Response False "No load is current.")
          @?= Exit "No load is current." Stderr (ExitFailure 1)
    , testCase "rejected argv goes to stderr with exit 1" $
        rejected "Usage: agda-mcp ..."
          @?= Exit "Usage: agda-mcp ..." Stderr (ExitFailure 1)
    , testCase "help goes to stdout with exit 0" $
        helped "agda-mcp" @?= Exit "agda-mcp" Stdout ExitSuccess
    , testCase "a plumbing failure goes to stderr with exit 2" $
        plumbing "daemon did not become ready"
          @?= Exit "daemon did not become ready" Stderr (ExitFailure 2)
    ]

-- Helpers

parse :: [String] -> Outcome
parse = parseCommandLine "/work"

invoke :: Text -> [(Text, Aeson.Value)] -> Outcome
invoke name arguments =
  Parsed defaultFlags defaultLifecycle (Invoke name (Map.fromList arguments))

loadId :: Aeson.Value
loadId = Aeson.String "L1-k7x2"

giveItem :: Int -> Text -> Maybe Text -> Map Text Aeson.Value
giveItem goal expression action =
  Map.fromList $
    [("goal", Aeson.toJSON goal), ("expression", Aeson.String expression)]
      <> foldMap (\a -> [("action", Aeson.String a)]) action

expectRejected :: Outcome -> IO ()
expectRejected (Rejected _) = pure ()
expectRejected other = assertFailure $ "expected a rejection, got " <> show other

assertContains :: Text -> Text -> IO ()
assertContains needle haystack =
  assertBool
    (show needle <> " not found in " <> show haystack)
    (needle `Text.isInfixOf` haystack)
