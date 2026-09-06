{-# LANGUAGE OverloadedStrings #-}

module Test.Interact.CLI (tests) where

import Data.Aeson qualified as Aeson
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import System.Exit (ExitCode (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import AgdaMCP.Interact.CLI (
  Exit (..),
  Format (..),
  Invocation (..),
  Mode (..),
  Outcome (..),
  Stream (..),
  answered,
  defaultIdleTimeout,
  helped,
  parseCommandLine,
  plumbing,
  rejected,
 )
import AgdaMCP.Interact.Daemon.Protocol (Response (..))
import AgdaMCP.Options (AgdaFlags (..), defaultFlags, renderFlags)
import AgdaMCP.Tools (goalTool)
import AgdaMCP.Tools.Description (overview, parameterDescription)
import AgdaMCP.Tools.Tool (Tool (..))

tests :: TestTree
tests =
  testGroup
    "CLI"
    [ verbTests
    , optionTests
    , globalTests
    , helpTests
    , exitTests
    ]

-- Verbs

verbTests :: TestTree
verbTests =
  testGroup
    "verbs"
    [ testCase "no verb is rejected" $
        expectRejected $
          parse []
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
        parse ["daemon"] @?= Parsed (invocation RunDaemon)
    , testCase "stop" $
        parse ["stop"] @?= Parsed (invocation Stop)
    , testCase "status" $
        parse ["status"] @?= Parsed (invocation Status)
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
            (invocation (Invoke "load" (Map.fromList [("path", Aeson.String "/x/X.agda")])))
              { invocationFlags =
                  defaultFlags
                    { includes = ["src"]
                    , libraries = ["standard-library"]
                    , defaultLibs = False
                    }
              }
    , testCase "Agda flags with no verb are rejected" $
        expectRejected $
          parse ["--library-file", "/x/libraries", "--no-libraries"]
    , testCase "idle timeout in minutes" $
        parse ["--idle-timeout", "90m", "status"]
          @?= Parsed (invocation Status) {invocationIdleTimeout = 5400}
    , testCase "idle timeout in seconds" $
        parse ["--idle-timeout", "10s", "daemon"]
          @?= Parsed (invocation RunDaemon) {invocationIdleTimeout = 10}
    , testCase "idle timeout in hours" $
        parse ["--idle-timeout", "3h", "daemon"]
          @?= Parsed (invocation RunDaemon) {invocationIdleTimeout = 10800}
    , testCase "the default idle timeout is two hours" $
        defaultIdleTimeout @?= 7200
    , testCase "an idle timeout without a unit is rejected" $
        expectRejected $
          parse ["--idle-timeout", "90", "status"]
    , testCase "the default format is text" $
        invocationFormat (invocation Status) @?= FormatText
    , testCase "json before the verb selects JSON output" $
        parse ["--json", "status"]
          @?= Parsed (invocation Status) {invocationFormat = FormatJson}
    , testCase "json sits among the other global flags in any order" $
        parse ["-i", "src", "--json", "--idle-timeout", "10s", "status"]
          @?= Parsed
            (invocation Status)
              { invocationFlags = defaultFlags {includes = ["src"]}
              , invocationIdleTimeout = 10
              , invocationFormat = FormatJson
              }
    , testCase "json after the verb selects JSON output" $
        parse ["status", "--json"]
          @?= Parsed (invocation Status) {invocationFormat = FormatJson}
    , testCase "json in both positions selects JSON output" $
        parse ["--json", "load", "/x/X.agda", "--json"]
          @?= Parsed
            (invocation (Invoke "load" (Map.fromList [("path", Aeson.String "/x/X.agda")])))
              { invocationFormat = FormatJson
              }
    , testCase "a verb without json prints text" $
        parse ["load", "/x/X.agda"]
          @?= Parsed
            (invocation (Invoke "load" (Map.fromList [("path", Aeson.String "/x/X.agda")])))
              { invocationFormat = FormatText
              }
    , testCase "json after daemon is rejected" $
        expectRejected $
          parse ["daemon", "--json"]
    , testCase "Agda flags after the verb are rejected" $
        expectRejected $
          parse ["status", "-i", "src"]
    , testCase "rendering the flags inverts parsing them" $
        parse (renderFlags flags <> ["status"])
          @?= Parsed (invocation Status) {invocationFlags = flags}
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

-- Help

helpTests :: TestTree
helpTests =
  testGroup
    "help"
    [ testCase "the top-level help names the program" $
        case parse ["--help"] of
          Helped text -> assertContains "agda-interact" text
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
    , testCase "the top-level help names the json flag" $
        case parse ["--help"] of
          Helped text -> assertContains "--json" text
          other -> assertFailure $ "expected help, got " <> show other
    , testCase "every answering verb's help names the json flag" $
        mapM_
          ( \verb -> case parse [verb, "--help"] of
              Helped text -> assertContains "--json" text
              other -> assertFailure $ "expected help for " <> verb <> ", got " <> show other
          )
          ["load", "goal", "check", "infer", "give", "case-split", "stop", "status"]
    , testCase "daemon's help does not name the json flag" $
        case parse ["daemon", "--help"] of
          Helped text -> assertLacks "--json" text
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
    , testCase "the top-level help opens with the overview" $
        case parse ["--help"] of
          Helped text -> assertContainsProse (firstSentence overview) text
          other -> assertFailure $ "expected help, got " <> show other
    , testCase "the top-level help spells case-split as the shell does" $
        case parse ["--help"] of
          Helped text -> do
            assertLacks "case_split" text
            assertContainsProse "fill them with `give` and `case-split`." text
          other -> assertFailure $ "expected help, got " <> show other
    , testCase "give's and case-split's help spell case-split as the shell does" $
        mapM_
          ( \verb -> case parse [verb, "--help"] of
              Helped text -> assertLacks "case_split" text
              other -> assertFailure $ "expected help for " <> verb <> ", got " <> show other
          )
          ["give", "case-split"]
    , testCase "goal's help keeps its field and kind names" $
        case parse ["goal", "--help"] of
          Helped text -> do
            assertContains "\"current_load_id\"" text
            assertContains "\"kind\": \"stale_load_id\"" text
          other -> assertFailure $ "expected help, got " <> show other
    , testCase "the top-level help shows the JSON envelope" $
        case parse ["--help"] of
          Helped text -> assertContains "{\"ok\": B, \"value\": V}" text
          other -> assertFailure $ "expected help, got " <> show other
    , testCase "the top-level help lists the shared refusals" $
        case parse ["--help"] of
          Helped text -> assertContains "\"kind\": \"stale_load_id\"" text
          other -> assertFailure $ "expected help, got " <> show other
    , testCase "goal's help describes its goal argument as the tool does" $
        case parse ["goal", "--help"] of
          Helped text ->
            assertContainsProse
              (parameterDescription "goal" (toolDescription goalTool))
              text
          other -> assertFailure $ "expected help, got " <> show other
    , testCase "goal's help lists its answers" $
        case parse ["goal", "--help"] of
          Helped text -> assertContains "\"kind\": \"goal\"" text
          other -> assertFailure $ "expected help, got " <> show other
    , testCase "give's help lists its outcomes" $
        case parse ["give", "--help"] of
          Helped text -> do
            assertContains "OUTCOME is one of:" text
            assertContains "\"kind\": \"applied\"" text
          other -> assertFailure $ "expected help, got " <> show other
    ]
 where
  firstSentence = (<> ".") . Text.takeWhile (/= '.')

-- Exit mapping

exitTests :: TestTree
exitTests =
  testGroup
    "exit"
    [ testCase "a successful answer goes to stdout with exit 0" $
        answered FormatText (Response True "Loaded." loaded)
          @?= Exit "Loaded." Stdout ExitSuccess
    , testCase "a refused answer goes to stderr with exit 1" $
        answered FormatText (Response False "No load is current." refusal)
          @?= Exit "No load is current." Stderr (ExitFailure 1)
    , testCase "as JSON, a successful answer is the envelope on stdout with exit 0" $ do
        let Exit text stream code = answered FormatJson (Response True "Loaded." loaded)
        stream @?= Stdout
        code @?= ExitSuccess
        decoded text @?= Just (envelope True loaded)
    , testCase "as JSON, a refused answer is the envelope on stdout with exit 1" $ do
        let Exit text stream code =
              answered FormatJson (Response False "No load is current." refusal)
        stream @?= Stdout
        code @?= ExitFailure 1
        decoded text @?= Just (envelope False refusal)
    , testCase "rejected argv goes to stderr with exit 1" $
        rejected "Usage: agda-interact ..."
          @?= Exit "Usage: agda-interact ..." Stderr (ExitFailure 1)
    , testCase "help goes to stdout with exit 0" $
        helped "agda-interact" @?= Exit "agda-interact" Stdout ExitSuccess
    , testCase "a plumbing failure goes to stderr with exit 2" $
        plumbing "daemon did not become ready"
          @?= Exit "daemon did not become ready" Stderr (ExitFailure 2)
    ]
 where
  loaded =
    Aeson.object
      [ "kind" Aeson..= Aeson.String "loaded"
      , "load_id" Aeson..= Aeson.String "L1-k7x2"
      ]
  refusal =
    Aeson.object
      [ "kind" Aeson..= Aeson.String "no_current_load"
      , "message" Aeson..= Aeson.String "No load is current."
      ]
  envelope ok value = Aeson.object ["ok" Aeson..= ok, "value" Aeson..= value]
  decoded :: Text -> Maybe Aeson.Value
  decoded = Aeson.decodeStrict . encodeUtf8

-- Helpers

parse :: [String] -> Outcome
parse = parseCommandLine "/work"

invocation :: Mode -> Invocation
invocation mode =
  Invocation
    { invocationFlags = defaultFlags
    , invocationIdleTimeout = defaultIdleTimeout
    , invocationFormat = FormatText
    , invocationMode = mode
    }

invoke :: Text -> [(Text, Aeson.Value)] -> Outcome
invoke name arguments =
  Parsed $ invocation $ Invoke name (Map.fromList arguments)

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

assertLacks :: Text -> Text -> IO ()
assertLacks needle haystack =
  assertBool
    (show needle <> " found in " <> show haystack)
    (not $ needle `Text.isInfixOf` haystack)

-- | Containment up to line wrapping.
assertContainsProse :: Text -> Text -> IO ()
assertContainsProse needle haystack =
  assertContains (unwrap needle) (unwrap haystack)
 where
  unwrap = Text.unwords . Text.words
