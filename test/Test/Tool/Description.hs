{-# LANGUAGE OverloadedStrings #-}

module Test.Tool.Description (tests) where

import Control.Monad (forM_, unless)
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

import AgdaMCP.Tools (allTools)
import AgdaMCP.Tools.Description (
  Description (..),
  Field (..),
  Shape (..),
  Variant (..),
  invalidArguments,
  namedShapes,
  timedOut,
 )
import AgdaMCP.Tools.Tool (Tool (..))

tests :: TestTree
tests =
  testGroup
    "Description"
    [ testCase "the tools are named and ordered as the MCP server lists them" $
        map descriptionName descriptions
          @?= ["load", "goal", "check", "infer", "give", "case_split"]
    , testCase "every parameter is described, nested objects included" $
        forM_ descriptions $ \description ->
          forM_ (undescribed $ descriptionParameters description) $ \name ->
            assertFailure $
              Text.unpack (descriptionName description)
                <> " leaves "
                <> Text.unpack name
                <> " undescribed"
    , testCase "every named shape resolves" $
        forM_ descriptions $ \description ->
          forM_ (mentioned description) $ \name ->
            assertBool
              (Text.unpack name <> " is not a named shape")
              (name `elem` map fst namedShapes)
    , testCase "every refusal carries its message" $
        forM_ descriptions $ \description ->
          forM_ (descriptionRefusals description) $ \variant ->
            unless (carriesMessage variant) $
              assertFailure $
                Text.unpack (variantKind variant) <> " has no message"
    , testCase "the adapter's answers carry their message" $
        forM_ [invalidArguments, timedOut] $ \variant ->
          assertBool
            (Text.unpack (variantKind variant) <> " has no message")
            (carriesMessage variant)
    , testCase "each tool refuses what its load id and goal admit" $
        map
          (\d -> (descriptionName d, map variantKind (descriptionRefusals d)))
          descriptions
          @?= [ ("load", [])
              , ("goal", goalRefusals)
              , ("check", goalRefusals)
              , ("infer", goalRefusals)
              , ("give", loadIdRefusals)
              , ("case_split", loadIdRefusals)
              ]
    , testCase "the shared leaves are the ones the values name" $
        map fst namedShapes
          @?= ["POSITION", "SPAN", "SHAPE", "ENTRY", "ERROR", "WARNING", "HIDDEN", "GOAL"]
    ]
 where
  descriptions = map toolDescription allTools
  loadIdRefusals = ["no_current_load", "stale_load_id", "foreign_load_id"]
  goalRefusals =
    loadIdRefusals <> ["source_changed", "source_unreadable", "unknown_goal"]

carriesMessage :: Variant -> Bool
carriesMessage variant =
  any (\f -> fieldName f == "message" && fieldShape f == ShapeString) $
    variantFields variant

-- | The names of parameters, at any depth, that have no description.
undescribed :: [Field] -> [Text]
undescribed = concatMap each
 where
  each f =
    [fieldName f | fieldDescription f == Nothing] <> nested (fieldShape f)
  nested (ShapeObject fields) = undescribed fields
  nested (ShapeArray shape) = nested shape
  nested (ShapeNullable shape) = nested shape
  nested _ = []

-- | Every name a description refers to through `ShapeNamed`.
mentioned :: Description -> [Text]
mentioned description =
  concatMap (inField . fieldShape) (descriptionParameters description)
    <> concatMap inVariant (descriptionAnswers description)
    <> concatMap inVariant (descriptionRefusals description)
 where
  inVariant = concatMap (inField . fieldShape) . variantFields
  inField (ShapeNamed name) = [name]
  inField (ShapeArray shape) = inField shape
  inField (ShapeNullable shape) = inField shape
  inField (ShapeObject fields) = concatMap (inField . fieldShape) fields
  inField (ShapeSum _ variants) = concatMap inVariant variants
  inField _ = []
