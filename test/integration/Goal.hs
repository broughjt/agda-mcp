{-# LANGUAGE OverloadedStrings #-}

-- Integration coverage for the `goal` tool: the M4 acceptance list (types at
-- normalization levels, expression quadrants, goal-specific constraints, and
-- the refusal paths).
module Goal (tests) where

import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import Agda.Interaction.Base (Rewrite (Normalised))
import Agda.Syntax.Common (InteractionId (InteractionId))

import AgdaMCP.Tools.Common (AgdaError (..))
import AgdaMCP.Tools.Goal (
  GoalDetail (..),
  GoalDisplay (..),
  GoalRequest (..),
  GoalResponse (..),
  GoalType (..),
  goal,
 )
import AgdaMCP.Tools.Load (
  ContextEntry (..),
  ContextEntryAttributes (..),
  Goal (..),
  GoalShape (..),
  LoadRequest (..),
  load,
 )

import Common (expectLoaded, runSession, withFixture)

tests :: TestTree
tests =
  testGroup
    "goal"
    [ testCase "goal type is reported as stated plus normalised when they differ" $
        withFixture "Normalize.agda" $ \path -> do
          response <-
            runSession $
              load (LoadRequest path)
                *> goal (GoalRequest path (InteractionId 0) Nothing Nothing)
          display <- expectDisplayed response
          displayGoal display @?= InteractionId 0
          displayType display
            @?= GoalType
              (GoalOfType "double zero ≡ plus zero zero")
              (Just (GoalOfType "zero ≡ zero"))
          displayDetail display @?= PlainGoal [] [] []
    , testCase "requested normalization reports that level only" $
        withFixture "Normalize.agda" $ \path -> do
          response <-
            runSession $
              load (LoadRequest path)
                *> goal (GoalRequest path (InteractionId 0) (Just Normalised) Nothing)
          display <- expectDisplayed response
          displayType display @?= GoalType (GoalOfType "zero ≡ zero") Nothing
    , testCase "plain query carries the goal's context" $
        withFixture "Context.agda" $ \path -> do
          response <-
            runSession $
              load (LoadRequest path)
                *> goal (GoalRequest path (InteractionId 1) Nothing Nothing)
          display <- expectDisplayed response
          displayType display @?= GoalType (GoalOfType "Nat") (Just (GoalOfType "Nat"))
          case displayDetail display of
            PlainGoal context boundary constraints -> do
              context
                @?= [ ContextEntry "x" True "x" "Nat" Nothing True noAttributes
                    , ContextEntry "y" True "y" "Nat" Nothing True noAttributes
                    ]
              boundary @?= []
              constraints @?= []
            other -> assertFailure ("expected a plain goal display, got " <> show other)
    , testCase "fitting expression infers and checks" $
        withFixture "Context.agda" $ \path -> do
          response <-
            runSession $
              load (LoadRequest path)
                *> goal (GoalRequest path (InteractionId 0) Nothing (Just "y"))
          display <- expectDisplayed response
          displayType display @?= GoalType (GoalOfType "Nat") (Just (GoalOfType "Nat"))
          displayDetail display @?= ExpressionGoal "y" (Right "Nat") (Right "y")
    , testCase "type mismatch infers but fails to check" $
        withFixture "Normalize.agda" $ \path -> do
          response <-
            runSession $
              load (LoadRequest path)
                *> goal (GoalRequest path (InteractionId 0) Nothing (Just "zero"))
          display <- expectDisplayed response
          case displayDetail display of
            ExpressionGoal submitted have checks -> do
              submitted @?= "zero"
              have @?= Right "Nat"
              case checks of
                Left e -> do
                  -- The noRange artifact: expression errors carry positions
                  -- relative to the submitted expression, not the file.
                  agdaErrorSpan e @?= Nothing
                  assertContains "UnequalTerms" (agdaErrorMessage e)
                Right term ->
                  assertFailure ("expected the check to fail, got " <> show term)
            other ->
              assertFailure ("expected an expression display, got " <> show other)
    , testCase "lambda infers a fresh-meta type while checking succeeds" $
        -- The M4 spec expected unannotated lambdas to fail inference; Agda
        -- 2.8.0 instead postpones and reports a fresh-meta function type, so
        -- this pins the real behavior.
        withFixture "Lambda.agda" $ \path -> do
          response <-
            runSession $
              load (LoadRequest path)
                *> goal (GoalRequest path (InteractionId 0) Nothing (Just "λ n → n"))
          display <- expectDisplayed response
          displayType display
            @?= GoalType (GoalOfType "Nat → Nat") (Just (GoalOfType "Nat → Nat"))
          displayDetail display
            @?= ExpressionGoal "λ n → n" (Right "(n : _8) → _8") (Right "λ n → n")
    , testCase "scope error fails both quadrants independently" $
        withFixture "Normalize.agda" $ \path -> do
          response <-
            runSession $
              load (LoadRequest path)
                *> goal (GoalRequest path (InteractionId 0) Nothing (Just "nope"))
          display <- expectDisplayed response
          case displayDetail display of
            ExpressionGoal _ (Left haveError) (Left checksError) -> do
              agdaErrorSpan haveError @?= Nothing
              assertContains "NotInScope" (agdaErrorMessage haveError)
              assertContains "NotInScope" (agdaErrorMessage checksError)
            other ->
              assertFailure
                ("expected both quadrants to fail, got " <> show other)
    , testCase "constraints mentioning the goal are reported" $
        -- The dogfood UX-gap-3 shape: a hole in type position whose clauses
        -- constrain it. The postponed definition check is attached to the
        -- goal (`getConstraintsMentioning`, InteractionTop.hs:1065).
        withFixture "Constraints.agda" $ \path -> do
          response <-
            runSession $
              load (LoadRequest path)
                *> goal (GoalRequest path (InteractionId 0) Nothing Nothing)
          display <- expectDisplayed response
          case displayDetail display of
            PlainGoal _ _ [constraint] ->
              assertContains "Check definition of Constraints.f" constraint
            other ->
              assertFailure ("expected one goal constraint, got " <> show other)
    , testCase "bogus goal id yields GoalUnknown" $
        withFixture "Normalize.agda" $ \path -> do
          response <-
            runSession $
              load (LoadRequest path)
                *> goal (GoalRequest path (InteractionId 9) Nothing Nothing)
          response @?= GoalUnknown (InteractionId 9)
    , testCase "bogus goal id with an expression yields GoalUnknown" $
        withFixture "Normalize.agda" $ \path -> do
          response <-
            runSession $
              load (LoadRequest path)
                *> goal (GoalRequest path (InteractionId 9) Nothing (Just "zero"))
          response @?= GoalUnknown (InteractionId 9)
    , testCase "unloaded file is refused with a fresh load" $
        withFixture "Normalize.agda" $ \path -> do
          response <-
            runSession $ goal (GoalRequest path (InteractionId 0) Nothing Nothing)
          case response of
            GoalNotLoaded reload -> do
              (goals, _, _, _) <- expectLoaded reload
              map goalId goals @?= [InteractionId 0]
            other -> assertFailure ("expected GoalNotLoaded, got " <> show other)
    , testCase "displaced file is refused" $
        -- Loading another file destroys the first file's interaction points,
        -- so a goal query against the displaced file is refused.
        withFixture "Normalize.agda" $ \path ->
          withFixture "Context.agda" $ \other -> do
            response <-
              runSession $
                load (LoadRequest path)
                  *> load (LoadRequest other)
                  *> goal (GoalRequest path (InteractionId 0) Nothing Nothing)
            case response of
              GoalNotLoaded _ -> pure ()
              wrong -> assertFailure ("expected GoalNotLoaded, got " <> show wrong)
    ]

expectDisplayed :: GoalResponse -> IO GoalDisplay
expectDisplayed (GoalDisplayed display) = pure display
expectDisplayed other =
  assertFailure ("expected GoalDisplayed, got " <> show other)

assertContains :: Text.Text -> Text.Text -> IO ()
assertContains fragment text
  | fragment `Text.isInfixOf` text = pure ()
  | otherwise =
      assertFailure
        ( "expected the text to mention "
            <> Text.unpack fragment
            <> ", got: "
            <> Text.unpack text
        )

noAttributes :: ContextEntryAttributes
noAttributes = ContextEntryAttributes Nothing False Nothing Nothing False
