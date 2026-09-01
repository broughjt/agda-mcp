{-# LANGUAGE OverloadedStrings #-}

module Test.Interact.Help (tests) where

import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import AgdaMCP.Interact.Help (
  outputSection,
  renderShape,
  renderVariant,
  shapesSection,
  status,
  stopping,
  unknownTool,
 )
import AgdaMCP.Tools.Description (
  Description (..),
  Effect (..),
  Field (..),
  Shape (..),
  Variant (..),
  field,
 )

tests :: TestTree
tests =
  testGroup
    "Help"
    [ shapeTests
    , variantTests
    , sectionTests
    , daemonTests
    ]

shapeTests :: TestTree
shapeTests =
  testGroup
    "renderShape"
    [ testCase "a string" $ renderShape ShapeString @?= "T"
    , testCase "an integer" $ renderShape ShapeInteger @?= "N"
    , testCase "a boolean" $ renderShape ShapeBoolean @?= "B"
    , testCase "an enum is its alternatives" $
        renderShape (ShapeEnum ["asis", "normalized"]) @?= "\"asis\" | \"normalized\""
    , testCase "an array" $ renderShape (ShapeArray ShapeString) @?= "[T]"
    , testCase "an object is its fields in order" $
        renderShape
          (ShapeObject [field "goal" ShapeInteger, field "span" (ShapeNamed "SPAN")])
          @?= "{\"goal\": N, \"span\": SPAN}"
    , testCase "a nullable" $
        renderShape (ShapeNullable (ShapeNamed "SPAN")) @?= "SPAN | null"
    , testCase "a named leaf is its name" $
        renderShape (ShapeNamed "ENTRY") @?= "ENTRY"
    , testCase "a nested sum is its name" $
        renderShape (ShapeSum "OUTCOME" outcomes) @?= "OUTCOME"
    ]

variantTests :: TestTree
variantTests =
  testGroup
    "renderVariant"
    [ testCase "kind first, then the fields" $
        renderVariant
          (Variant "failed" "Agda rejected it" [field "error" (ShapeNamed "ERROR")])
          @?= "{\"kind\": \"failed\", \"error\": ERROR}"
    , testCase "a variant with no fields is its kind alone" $
        renderVariant (Variant "file_changed" "the file changed" [])
          @?= "{\"kind\": \"file_changed\"}"
    ]

sectionTests :: TestTree
sectionTests =
  testGroup
    "sections"
    [ testCase "answers, refusals, then each nested sum once" $
        outputSection sample
          @?= Text.unlines
            [ "Output:"
            , "  Answers, exit 0:"
            , "    {\"kind\": \"completed\", \"outcome\": OUTCOME, \"reload\": LOAD}"
            , "        the edit and the reload"
            , "  Refusals, exit 1:"
            , "    {\"kind\": \"no_current_load\", \"message\": T}"
            , "        no load is current"
            , "  OUTCOME is one of:"
            , "    {\"kind\": \"applied\", \"edits\": [{\"goal\": N}]}"
            , "        edits were written"
            , "    {\"kind\": \"file_changed\"}"
            , "        the file changed on disk"
            , "  LOAD is one of:"
            , "    {\"kind\": \"loaded\", \"load_id\": T}"
            , "        the file checked"
            ]
    , testCase "a tool that refuses nothing has no refusals block" $
        outputSection sample {descriptionAnswers = loads, descriptionRefusals = []}
          @?= Text.unlines
            [ "Output:"
            , "  Answers, exit 0:"
            , "    {\"kind\": \"loaded\", \"load_id\": T}"
            , "        the file checked"
            ]
    , testCase "the named leaves are a two-column table" $
        shapesSection
          [
            ( "POSITION"
            , ShapeObject [field "offset" ShapeInteger, field "line" ShapeInteger]
            )
          ,
            ( "SPAN"
            , ShapeObject
                [field "start" (ShapeNamed "POSITION"), field "end" (ShapeNamed "POSITION")]
            )
          ]
          @?= Text.unlines
            [ "Shapes named in verb output:"
            , "  POSITION  {\"offset\": N, \"line\": N}"
            , "  SPAN      {\"start\": POSITION, \"end\": POSITION}"
            ]
    ]

daemonTests :: TestTree
daemonTests =
  testGroup
    "daemon answers"
    [ testCase "an unknown tool is a refusal naming the tool" $ do
        variantKind unknownTool @?= "unknown_tool"
        map fieldName (variantFields unknownTool) @?= ["tool", "message"]
    , testCase "status carries the load or null" $ do
        variantKind status @?= "status"
        map fieldName (variantFields status) @?= ["load"]
        assertBool "load is nullable" $ case map fieldShape (variantFields status) of
          [ShapeNullable _] -> True
          _ -> False
    , testCase "stopping carries nothing" $ do
        variantKind stopping @?= "stopping"
        variantFields stopping @?= []
    ]

-- Fixtures

sample :: Description
sample =
  Description
    { descriptionName = "sample"
    , descriptionSummary = "A sample."
    , descriptionParameters = []
    , descriptionAnswers =
        [ Variant
            "completed"
            "the edit and the reload"
            [ field "outcome" (ShapeSum "OUTCOME" outcomes)
            , field "reload" (ShapeSum "LOAD" loads)
            ]
        ]
    , descriptionRefusals =
        [Variant "no_current_load" "no load is current" [field "message" ShapeString]]
    , descriptionEffect = Edits
    }

outcomes :: [Variant]
outcomes =
  [ Variant
      "applied"
      "edits were written"
      [field "edits" (ShapeArray (ShapeObject [field "goal" ShapeInteger]))]
  , Variant "file_changed" "the file changed on disk" []
  ]

loads :: [Variant]
loads = [Variant "loaded" "the file checked" [field "load_id" ShapeString]]
