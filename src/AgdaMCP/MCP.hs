{-# LANGUAGE OverloadedStrings #-}

-- | The MCP front end: the server's handlers over the tool table, and its command line.
module AgdaMCP.MCP (
  handlers,
  serverInfo,
  toolDefinition,
  Outcome (..),
  parseOptions,
) where

import Agda.Interaction.Options (CommandLineOptions)
import Control.Concurrent.MVar (MVar)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import MCP.Server (
  Content (..),
  Error (..),
  McpServerHandlers (..),
  McpServerInfo (..),
  Schema,
  SchemaType (..),
  ToolAnnotations (..),
  ToolDefinition (..),
  ToolResult (..),
  defaultToolAnnotations,
  describedSchema,
  mkToolDefinition,
  noHandlers,
  schema,
  toolResult,
 )
import Options.Applicative (
  ParserInfo,
  ParserResult (CompletionInvoked, Failure, Success),
  defaultPrefs,
  execParserPure,
  fullDesc,
  header,
  help,
  info,
  long,
  renderFailure,
  short,
 )
import Options.Applicative.Extra (helperWith)
import System.Exit (ExitCode (ExitSuccess))

import AgdaMCP.Options (AgdaFlags, flagsParser, toCommandLineOptions)
import AgdaMCP.Tools (allTools)
import AgdaMCP.Tools.Description (
  Description (..),
  Effect (..),
  Field (..),
  Shape (..),
  overview,
 )
import AgdaMCP.Tools.State (ToolState)
import AgdaMCP.Tools.Tool (Answer (..), Tool (..))

serverInfo :: McpServerInfo
serverInfo =
  McpServerInfo
    { serverName = "agda-mcp"
    , serverVersion = "0.1.0.0"
    , serverInstructions = overview
    }

handlers :: MVar ToolState -> McpServerHandlers
handlers stateVariable =
  noHandlers {tools = Just (list, call)}
 where
  byName :: Map Text Tool
  byName =
    Map.fromList
      [(descriptionName $ toolDescription tool, tool) | tool <- allTools]

  list _ = pure $ map toolDefinition allTools

  call _ name arguments =
    maybe
      (pure $ Left $ UnknownTool name)
      (\tool -> Right . toolResultOf <$> toolCall tool stateVariable arguments)
      (Map.lookup name byName)

  -- The rendered text as the tool's one content item, with `isError` following
  -- `answerOk`, exactly as `toolResult` and `toolError` build it.
  toolResultOf :: Answer -> ToolResult
  toolResultOf answer =
    (toolResult [ContentText $ answerText answer])
      { toolResultIsError = not $ answerOk answer
      }

{- | The tool's MCP definition, from its description: the summary, an input
schema from its parameters, and annotations from its effect. The output schema
is left unset.
-}
toolDefinition :: Tool -> ToolDefinition
toolDefinition tool =
  ( mkToolDefinition
      (descriptionName description)
      (descriptionSummary description)
      (schema $ objectType $ descriptionParameters description)
  )
    { toolDefinitionAnnotations = Just $ annotations $ descriptionEffect description
    }
 where
  description = toolDescription tool

objectType :: [Field] -> SchemaType
objectType fields =
  SchemaObject
    [(fieldName f, fieldSchema f) | f <- fields]
    [fieldName f | f <- fields, fieldRequired f]

fieldSchema :: Field -> Schema
fieldSchema f =
  maybe schema describedSchema (fieldDescription f) $ shapeType $ fieldShape f

{- | Only the shapes a parameter takes. The output-only constructors have no
input schema, so a parameter that used one fails at `tools/list`, which the
golden catches.
-}
shapeType :: Shape -> SchemaType
shapeType ShapeString = SchemaString Nothing
shapeType (ShapeEnum alternatives) = SchemaString $ Just alternatives
shapeType ShapeInteger = SchemaInteger
shapeType ShapeBoolean = SchemaBoolean
shapeType (ShapeArray element) = SchemaArray $ schema $ shapeType element
shapeType (ShapeObject fields) = objectType fields
shapeType shape = error $ "a parameter has no input schema: " <> show shape

annotations :: Effect -> ToolAnnotations
annotations Inspects =
  defaultToolAnnotations
    { toolReadOnlyHint = Just True
    , toolIdempotentHint = Just True
    , toolOpenWorldHint = Just False
    }
annotations Loads =
  defaultToolAnnotations
    { toolReadOnlyHint = Just True
    , toolOpenWorldHint = Just False
    }
annotations Edits =
  defaultToolAnnotations
    { toolReadOnlyHint = Just False
    , toolDestructiveHint = Just True
    , toolIdempotentHint = Just False
    , toolOpenWorldHint = Just False
    }

data Outcome
  = Parsed CommandLineOptions
  | Helped Text
  | Rejected Text

parseOptions :: [String] -> Outcome
parseOptions arguments =
  case execParserPure defaultPrefs information arguments of
    Success flags -> Parsed $ toCommandLineOptions flags
    Failure failure -> case renderFailure failure programName of
      (text, ExitSuccess) -> Helped $ trimmed text
      (text, _) -> Rejected $ trimmed text
    CompletionInvoked _ ->
      Rejected "agda-mcp provides no shell completion."
 where
  trimmed = Text.strip . Text.pack

programName :: String
programName = "agda-mcp"

information :: ParserInfo AgdaFlags
information =
  info
    (helperWith helpFlag <*> flagsParser)
    (fullDesc <> header "agda-mcp")
 where
  helpFlag = long "help" <> short 'h' <> help "show this help text"
