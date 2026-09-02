{-# LANGUAGE OverloadedStrings #-}

-- | The @agda-interact@ command line: its grammar, help and exit mapping.
module AgdaMCP.Interact.CLI (
  Mode (..),
  Format (..),
  Invocation (..),
  defaultIdleTimeout,
  Outcome (..),
  parseCommandLine,
  Stream (..),
  Exit (..),
  answered,
  helped,
  rejected,
  plumbing,
) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Text (encodeToLazyText)
import Data.Char (isDigit)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Options.Applicative (
  Mod,
  Parser,
  ParserInfo,
  ParserResult (CompletionInvoked, Failure, Success),
  ReadM,
  argument,
  command,
  eitherReader,
  execParserPure,
  flag,
  footerDoc,
  fullDesc,
  header,
  help,
  info,
  long,
  many,
  metavar,
  noBacktrack,
  option,
  optional,
  prefs,
  progDesc,
  renderFailure,
  short,
  strArgument,
  subparser,
  switch,
  value,
 )
import Options.Applicative.Extra (helperWith)
import Options.Applicative.Help.Pretty (Doc, pretty, vsep)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import Text.Read (readMaybe)

import AgdaMCP.Interact.Daemon.Protocol (Response (..))
import AgdaMCP.Interact.Help (
  outputSection,
  shapesSection,
  status,
  stopping,
  unknownTool,
  variantEntries,
 )
import AgdaMCP.Options (AgdaFlags, flagsParser)
import AgdaMCP.Tools (
  caseSplitTool,
  checkTool,
  giveTool,
  goalTool,
  inferTool,
  loadTool,
 )
import AgdaMCP.Tools.Description (
  Description (..),
  invalidArguments,
  loadIdRefusals,
  namedShapes,
  overview,
  parameterDescription,
  sourceRefusals,
  timedOut,
  unknownGoal,
 )
import AgdaMCP.Tools.Tool (Tool (..), normalizations)

data Mode
  = -- | The @daemon@ verb: serve the socket in the foreground.
    RunDaemon
  | {- | A tool verb: the tool's name and the arguments it would receive
    over MCP.
    -}
    Invoke Text (Map Text Value)
  | Stop
  | Status
  deriving (Eq, Show)

-- | How the client prints an answer. Either prose, or @{"ok": B, "value": V}@.
data Format = FormatText | FormatJson
  deriving (Eq, Show)

-- | Seconds without a request after which the daemon exits.
defaultIdleTimeout :: Int
defaultIdleTimeout = 2 * 60 * 60 -- 2 hours

data Invocation = Invocation
  { invocationFlags :: AgdaFlags
  , invocationIdleTimeout :: Int
  -- ^ Seconds; honoured by @daemon@ and forwarded to a spawn by other verbs.
  , invocationFormat :: Format
  , invocationMode :: Mode
  }
  deriving (Eq, Show)

data Outcome
  = Parsed Invocation
  | Helped Text
  | Rejected Text
  deriving (Eq, Show)

{- | Parse argv. The directory is the client's working directory, against which
@load@'s path is made absolute, since the daemon's is not the client's.
-}
parseCommandLine :: FilePath -> [String] -> Outcome
parseCommandLine directory arguments =
  -- Without backtracking, a verb's parser must consume everything after the
  -- verb, so a global flag placed after it is rejected rather than handed
  -- back to the outer parser.
  case execParserPure (prefs noBacktrack) (information directory) arguments of
    Success invocation -> Parsed invocation
    Failure failure -> case renderFailure failure programName of
      (text, ExitSuccess) -> Helped $ trimmed text
      (text, _) -> Rejected $ trimmed text
    CompletionInvoked _ ->
      Rejected $ Text.pack programName <> " provides no shell completion."
 where
  trimmed = Text.strip . Text.pack

programName :: String
programName = "agda-interact"

information :: FilePath -> ParserInfo Invocation
information directory =
  info
    (helper <*> invocationParser directory)
    ( fullDesc
        <> header (programName <> " - drive an Agda session from the shell")
        <> progDesc (Text.unpack overview)
        <> footerDoc (Just $ unwrapped topFooter)
    )

helper :: Parser (a -> a)
helper = helperWith $ long "help" <> short 'h' <> help "show this help text"

-- | Lines rendered as they are, since the output shapes must not wrap.
unwrapped :: Text -> Doc
unwrapped = vsep . map pretty . Text.lines

invocationParser :: FilePath -> Parser Invocation
invocationParser directory =
  Invocation
    <$> flagsParser
    <*> option
      duration
      ( long "idle-timeout"
          <> metavar "DURATION"
          <> value defaultIdleTimeout
          <> help
            "how long the daemon stays up without a request, as 30s, 90m or 2h; honoured by daemon, and passed along by any other verb that starts one (default 2h)"
      )
    <*> flag
      FormatText
      FormatJson
      ( long "json"
          <> help "print the answer as {\"ok\": B, \"value\": V} on stdout instead of prose"
      )
    <*> verbParser directory

-- | An integer with a unit: @s@, @m@ or @h@.
duration :: ReadM Int
duration = eitherReader $ \text ->
  case span isDigit text of
    (digits@(_ : _), [unit])
      | Just multiplier <- lookup unit [('s', 1), ('m', 60), ('h', 3600)] ->
          Right $ read digits * multiplier
    _ -> Left "expected a duration with a unit, such as 30s, 90m or 2h"

verbParser :: FilePath -> Parser Mode
verbParser directory =
  subparser $
    metavar "VERB"
      <> toolVerb "load" loadTool [] (loadParser directory)
      <> toolVerb "goal" goalTool [] goalParser
      <> toolVerb "check" checkTool [dashNote "check"] checkParser
      <> toolVerb "infer" inferTool [dashNote "infer"] inferParser
      <> toolVerb "give" giveTool [dashNote "give", interruptNote] giveParser
      <> toolVerb "case-split" caseSplitTool [interruptNote] caseSplitParser
      <> verb
        "daemon"
        "Run the daemon in the foreground, serving this directory's project under the Agda flags given. The other verbs start one of these themselves when none is listening."
        (pure RunDaemon)
      <> verb
        "stop"
        "Ask the daemon to exit once its in-flight request finishes."
        (pure Stop)
      <> verb
        "status"
        "Report what the daemon has loaded, if anything."
        (pure Status)
 where
  toolVerb name tool notes parser =
    command name $
      info
        (helper <*> parser (toolDescription tool))
        ( progDesc (Text.unpack $ descriptionSummary $ toolDescription tool)
            <> footerDoc
              ( Just $
                  unwrapped $
                    Text.unlines (notes <> [""])
                      <> outputSection (toolDescription tool)
              )
        )
  verb name summary parser =
    command name $ info (helper <*> parser) (progDesc summary)

  dashNote name =
    "An expression beginning with a dash must follow a double dash: "
      <> Text.pack programName
      <> " "
      <> name
      <> " L1-k7x2 0 -- '-x'."
  interruptNote =
    "Interrupting the client does not cancel the edit: the daemon finishes the request and writes the file, so run status or load afterwards to see where it stands."

-- The tool verbs. Each assembles exactly the arguments the MCP layer would receive.

loadParser :: FilePath -> Description -> Parser Mode
loadParser directory description =
  ( \path timeout -> invoke "load" [("path", Just $ toJSON $ directory </> path), timeout]
  )
    <$> strArgument (metavar "PATH" <> parameterHelp "path" description)
    <*> timeoutOption description

goalParser :: Description -> Parser Mode
goalParser description =
  ( \loadId goal normalization timeout -> invoke "goal" [loadId, goal, normalization, timeout]
  )
    <$> loadIdArgument description
    <*> goalArgument description
    <*> normalizationOption description
    <*> timeoutOption description

checkParser :: Description -> Parser Mode
checkParser = expressionParser "check"

inferParser :: Description -> Parser Mode
inferParser = expressionParser "infer"

expressionParser :: Text -> Description -> Parser Mode
expressionParser name description =
  ( \loadId goal expression normalization timeout -> invoke name [loadId, goal, expression, normalization, timeout]
  )
    <$> loadIdArgument description
    <*> goalArgument description
    <*> expressionArgument description
    <*> normalizationOption description
    <*> timeoutOption description

giveParser :: Description -> Parser Mode
giveParser description =
  ( \loadId goal expression refine timeout -> invoke "give" [loadId, gives goal expression refine, timeout]
  )
    <$> loadIdArgument description
    <*> goalArgument description
    <*> expressionArgument description
    <*> switch
      ( long "refine"
          <> help
            "partially fill the goal, leaving new subgoals in its place, instead of checking the expression against it and writing it"
      )
    <*> timeoutOption description
 where
  -- The one-item batch of the tool's request, with the action omitted when it
  -- is the default, so that the tool's own defaulting applies.
  gives goal expression refine =
    ( "gives"
    , Just $
        toJSON
          [ object $
              [(Key.fromText name, v) | (name, Just v) <- [goal, expression]]
                <> [("action", String "refine") | refine]
          ]
    )

caseSplitParser :: Description -> Parser Mode
caseSplitParser description =
  ( \loadId goal variables timeout ->
      invoke
        "case_split"
        [loadId, goal, ("variables", Just $ toJSON variables), timeout]
  )
    <$> loadIdArgument description
    <*> goalArgument description
    <*> many
      ( strArgument (metavar "VARIABLE" <> parameterHelp "variables" description) ::
          Parser Text
      )
    <*> timeoutOption description

-- | A tool verb's mode: the arguments present, by their request field names.
invoke :: Text -> [(Text, Maybe Value)] -> Mode
invoke name fields =
  Invoke name $ Map.fromList [(field, v) | (field, Just v) <- fields]

parameterHelp :: Text -> Description -> Mod f a
parameterHelp name description =
  help $ Text.unpack $ parameterDescription name description

loadIdArgument :: Description -> Parser (Text, Maybe Value)
loadIdArgument description =
  (\loadId -> ("load_id", Just $ String loadId))
    <$> strArgument (metavar "LOAD_ID" <> parameterHelp "load_id" description)

-- | An integer, with a leading @?@ accepted since that is how Agda writes goals.
goalArgument :: Description -> Parser (Text, Maybe Value)
goalArgument description =
  (\goal -> ("goal", Just $ toJSON goal))
    <$> argument goalId (metavar "GOAL" <> parameterHelp "goal" description)
 where
  goalId :: ReadM Int
  goalId = eitherReader $ \text ->
    case readMaybe (dropWhile (== '?') text) of
      Just n | n >= (0 :: Int) -> Right n
      _ -> Left $ "expected a goal ID such as 0 or ?0, but got " <> show text

expressionArgument :: Description -> Parser (Text, Maybe Value)
expressionArgument description =
  (\expression -> ("expression", Just $ String expression))
    <$> strArgument (metavar "EXPRESSION" <> parameterHelp "expression" description)

-- | Sent only when given, so the tool's default stays the tool's.
normalizationOption :: Description -> Parser (Text, Maybe Value)
normalizationOption description =
  (\level -> ("normalization", String <$> level))
    <$> optional
      ( option
          normalization
          ( long "normalization"
              <> metavar "LEVEL"
              <> parameterHelp "normalization" description
          )
      )
 where
  normalization :: ReadM Text
  normalization = eitherReader $ \text ->
    let level = Text.pack text
     in if Map.member level normalizations
          then Right level
          else
            Left $
              "expected one of "
                <> Text.unpack (Text.intercalate ", " $ Map.keys normalizations)
                <> ", but got "
                <> show text

-- | In seconds, sent only when given.
timeoutOption :: Description -> Parser (Text, Maybe Value)
timeoutOption description =
  (\seconds -> ("timeout", toJSON <$> seconds))
    <$> optional
      ( option
          positive
          ( long "timeout"
              <> metavar "SECONDS"
              <> parameterHelp "timeout" description
          )
      )
 where
  positive :: ReadM Int
  positive = eitherReader $ \text ->
    case readMaybe text of
      Just n | n > 0 -> Right n
      _ ->
        Left $
          "expected a whole number of seconds greater than zero, but got " <> show text

-- | What the top-level help says after the flags: the envelope, the shared answers and the shapes.
topFooter :: Text
topFooter =
  Text.unlines $
    [ "Output:"
    , "  A verb prints its answer as prose: on stdout with exit 0 when the tool"
    , "  answered, on stderr with exit 1 when it refused. With --json the answer"
    , "  prints as {\"ok\": B, \"value\": V} on stdout either way, with the same exit"
    , "  code. Exit 2 means the daemon could not be reached and nothing was answered."
    , "  Each verb's own --help lists the values it answers."
    , ""
    , "  Refusals every verb that takes a load ID can answer, exit 1:"
    ]
      <> variantEntries (loadIdRefusals <> sourceRefusals <> [unknownGoal])
      <> [ "  Answers that are not a tool's; the last two are answered with exit 0:"
         ]
      <> variantEntries [invalidArguments, timedOut, unknownTool, status, stopping]
      <> [""]
      <> Text.lines (shapesSection namedShapes)

-- Exit mapping

data Stream = Stdout | Stderr
  deriving (Eq, Show)

-- | How a client invocation ends: what it prints, where, and the exit code.
data Exit = Exit
  { exitText :: Text
  , exitStream :: Stream
  , exitCode :: ExitCode
  }
  deriving (Eq, Show)

{- | The daemon answered. As text, a refusal or error goes to stderr with exit
1. As JSON, the envelope goes to stdout either way, with the same exit code.
-}
answered :: Format -> Response -> Exit
answered format response =
  case format of
    FormatText ->
      Exit
        { exitText = responseText response
        , exitStream = if ok then Stdout else Stderr
        , exitCode = code
        }
    FormatJson ->
      Exit
        { exitText =
            Text.Lazy.toStrict $
              encodeToLazyText $
                object ["ok" .= ok, "value" .= responseValue response]
        , exitStream = Stdout
        , exitCode = code
        }
 where
  ok = responseOk response
  code = if ok then ExitSuccess else ExitFailure 1

-- | Help text: stdout, exit 0.
helped :: Text -> Exit
helped text = Exit text Stdout ExitSuccess

-- | Argv rejected by the parser: stderr, exit 1.
rejected :: Text -> Exit
rejected text = Exit text Stderr (ExitFailure 1)

{- | Plumbing failed before the daemon answered: cannot connect or spawn, not
ready, replace failed, EOF after the hello, malformed response. stderr, exit 2.
-}
plumbing :: Text -> Exit
plumbing text = Exit text Stderr (ExitFailure 2)
