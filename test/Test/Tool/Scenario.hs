{-# LANGUAGE OverloadedStrings #-}

module Test.Tool.Scenario (tests) where

import Agda.Interaction.Base (Rewrite (..))
import Agda.Syntax.Common (InteractionId)
import Agda.TypeChecking.Monad (TCState)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

import AgdaMCP.Interaction (
  ContextEntry (..),
  Error (..),
  Goal (..),
  GoalReport (..),
  GoalShape (..),
 )
import AgdaMCP.Interaction.GoalInfer (Have (..))
import AgdaMCP.Interaction.MakeCase (MakeCaseVariant (..))
import AgdaMCP.Tools.CaseSplit qualified as CaseSplit
import AgdaMCP.Tools.Check qualified as Check
import AgdaMCP.Tools.Give qualified as Give
import AgdaMCP.Tools.Goal qualified as Goal
import AgdaMCP.Tools.Load qualified as Load
import AgdaMCP.Tools.LoadId (LoadId (..), LoadIdRefusal (..))
import Test.Tool.Harness (withFixtureToolSession)

tests :: IO TCState -> TestTree
tests warm = testGroup "Scenarios" [distributivity warm]

distributivity :: IO TCState -> TestTree
distributivity warm =
  testCase
    "proof that multiplication distributes over addition for natural numbers"
    $ withFixtureToolSession warm "test/fixtures/NatDistrib.agda"
    $ \path -> do
      do
        report <-
          Load.load Load.Request {Load.loadRequestPath = path}
            >>= liftIO . expectLoadOk "load"
        liftIO $ do
          Load.loadReportId report @?= LoadId 1
          Load.loadReportPath report @?= path
          goals report @?= [(0, GoalOfType "_*_ DistributesOverʳ _+_")]
          contexts report @?= [[]]
          onlyGoals report

      -- `Simplified` does not unfold a goal type stated as a definition.
      do
        report <-
          Goal.goal
            Goal.Request
              { Goal.goalRequestLoadId = LoadId 1
              , Goal.goalRequestGoalId = 0
              , Goal.goalRequestNormalization = Simplified
              }
            >>= liftIO . expectGoalOk "the goal at the default normalization"
        liftIO $ do
          goalReportShape report @?= GoalOfType "_*_ DistributesOverʳ _+_"
          bindings report @?= []

      do
        report <-
          Goal.goal
            Goal.Request
              { Goal.goalRequestLoadId = LoadId 1
              , Goal.goalRequestGoalId = 0
              , Goal.goalRequestNormalization = Normalised
              }
            >>= liftIO . expectGoalOk "the goal normalized"
        liftIO $ do
          goalReportShape report
            @?= GoalOfType "(x y z : ℕ) → (y + z) * x ≡ y * x + z * x"
          bindings report @?= []

      do
        (edit, report) <-
          CaseSplit.caseSplit
            CaseSplit.Request
              { CaseSplit.caseSplitRequestLoadId = LoadId 1
              , CaseSplit.caseSplitRequestGoalId = 0
              , CaseSplit.caseSplitRequestVariables = []
              }
            >>= liftIO . expectSplitApplied "introduce the arguments"
        liftIO $ do
          CaseSplit.editVariant edit @?= MakeCaseFunction
          CaseSplit.editClauses edit @?= ["*-distribʳ-+ x y z = ?"]
          proofShouldBe path [signature, "*-distribʳ-+ x y z = ?"]
          Load.loadReportId report @?= LoadId 2
          Load.loadReportPath report @?= path
          goals report @?= [(0, GoalOfType "(y + z) * x ≡ y * x + z * x")]
          -- TODO: Revisit when writing load response renderer
          -- Outermost first, the reverse of what Agda's own buffer displays.
          -- The binders the split generated are written to the file before
          -- the resync reads them back, so they arrive in scope under their
          -- own names.
          contexts report @?= [[natural "x", natural "y", natural "z"]]
          onlyGoals report

      do
        refused <-
          Goal.goal
            Goal.Request
              { Goal.goalRequestLoadId = LoadId 1
              , Goal.goalRequestGoalId = 0
              , Goal.goalRequestNormalization = Simplified
              }
        liftIO $ refused @?= Goal.ResponseRefused (StaleLoadId $ LoadId 2)

      afterSplit <- do
        (edit, report) <-
          CaseSplit.caseSplit
            CaseSplit.Request
              { CaseSplit.caseSplitRequestLoadId = LoadId 2
              , CaseSplit.caseSplitRequestGoalId = 0
              , CaseSplit.caseSplitRequestVariables = ["y"]
              }
            >>= liftIO . expectSplitApplied "split on the recursive argument"
        liftIO $ do
          CaseSplit.editClauses edit
            @?= [ "*-distribʳ-+ x zero z = ?"
                , "*-distribʳ-+ x (suc y) z = ?"
                ]
          proofShouldBe path splitProof
          Load.loadReportId report @?= LoadId 3
          Load.loadReportPath report @?= path
          goals report
            @?= [ (0, GoalOfType "(zero + z) * x ≡ zero * x + z * x")
                , (1, GoalOfType "(suc y + z) * x ≡ suc y * x + z * x")
                ]
          contexts report
            @?= [ [natural "x", natural "z"]
                , [natural "x", natural "y", natural "z"]
                ]
          onlyGoals report
        pure report

      do
        report <-
          Goal.goal
            Goal.Request
              { Goal.goalRequestLoadId = LoadId 3
              , Goal.goalRequestGoalId = 0
              , Goal.goalRequestNormalization = Simplified
              }
            >>= liftIO . expectGoalOk "the base case"
        liftIO $ do
          goalReportShape report @?= GoalOfType "z * x ≡ z * x"
          bindings report @?= [natural "x", natural "z"]

      do
        (rejection, report) <-
          Give.give
            Give.Request
              { Give.giveRequestLoadId = LoadId 3
              , Give.giveRequestItems =
                  [ (0, Give.ActionGive "refl")
                  , (1, Give.ActionGive wrongStep)
                  ]
              }
            >>= liftIO . expectGiveCompleted "a batch guessing the step case"
        liftIO $ do
          case rejection of
            Give.OutcomeRefused refusal -> do
              Give.refusalGoalId refusal @?= 1
              Give.refusalPosition refusal @?= Give.BatchPosition 1 2
              case Give.refusalReason refusal of
                Give.RefusedError e ->
                  errorShouldBe
                    path
                    e
                    [ "FIXTURE:16.28-59: error: [UnequalTerms]"
                    , "x != x + y * x of type ℕ"
                    , "when checking that the inferred type of an application"
                    , "  x + (y + z) * x ≡ x + _y_12"
                    , "matches the expected type"
                    , "  (suc y + z) * x ≡ suc y * x + z * x"
                    ]
                other ->
                  assertFailure $ "unexpected refusal reason: " <> show other
            other -> assertFailure $ "unexpected outcome: " <> show other
          proofShouldBe path splitProof
          Load.loadReportId report @?= LoadId 4
          Load.loadReportPath report @?= path
          goals report @?= goals afterSplit
          contexts report @?= contexts afterSplit
          onlyGoals report

      do
        report <-
          Check.check
            Check.Request
              { Check.checkRequestLoadId = LoadId 4
              , Check.checkRequestGoalId = 1
              , Check.checkRequestNormalization = Simplified
              , Check.checkRequestExpression = wrongStep
              }
            >>= liftIO . expectCheckOk "check the rejected expression"
        liftIO $ do
          goalReportShape (Check.checkReportGoal report)
            @?= GoalOfType "x + (y + z) * x ≡ x + y * x + z * x"
          fmap haveType (Check.checkReportHave report)
            @?= Right "x + (y + z) * x ≡ x + (y * x + z * x)"
          case Check.checkReportChecks report of
            -- TODO: Revisit this behavior during implementation
            -- Check parses at `noRange`, so its positions are the
            -- expression's own rather than the file's.
            Left e ->
              errorShouldBe
                path
                e
                [ "1.1-32: error: [UnequalTerms]"
                , "x != x + y * x of type ℕ"
                , "when checking that the inferred type of an application"
                , "  x + (y + z) * x ≡ x + _y_15"
                , "matches the expected type"
                , "  (suc y + z) * x ≡ suc y * x + z * x"
                ]
            Right elaborated ->
              assertFailure $ "expected a failure: " <> show elaborated

      do
        report <-
          Check.check
            Check.Request
              { Check.checkRequestLoadId = LoadId 4
              , Check.checkRequestGoalId = 1
              , Check.checkRequestNormalization = Simplified
              , Check.checkRequestExpression = step
              }
            >>= liftIO . expectCheckOk "check the reassociated expression"
        liftIO $ do
          goalReportShape (Check.checkReportGoal report)
            @?= GoalOfType "x + (y + z) * x ≡ x + y * x + z * x"
          Check.checkReportExpression report @?= step
          fmap haveType (Check.checkReportHave report)
            @?= Right "x + (y + z) * x ≡ x + y * x + z * x"
          Check.checkReportChecks report @?= Right elaboratedStep

      do
        (applied, report) <-
          Give.give
            Give.Request
              { Give.giveRequestLoadId = LoadId 4
              , Give.giveRequestItems =
                  [ (0, Give.ActionGive "refl")
                  , (1, Give.ActionGive step)
                  ]
              }
            >>= liftIO . expectGiveCompleted "a batch filling both clauses"
        liftIO $ do
          case applied of
            Give.OutcomeApplied [baseEdit, stepEdit] -> do
              Give.editGoalId baseEdit @?= 0
              Give.editText baseEdit @?= "refl"
              Give.editKind baseEdit @?= Give.EditKindComputed
              Give.editGoalId stepEdit @?= 1
              Give.editText stepEdit @?= elaboratedStep
              Give.editKind stepEdit @?= Give.EditKindComputed
            other -> assertFailure $ "unexpected outcome: " <> show other
          proofShouldBe
            path
            [ signature
            , baseClause <> "refl"
            , stepClause <> "trans (cong (x +_) (*-distribʳ-+ x y z))"
            , Text.replicate (Text.length stepClause) " "
                <> "(sym (+-assoc x (y * x) (z * x)))"
            ]
          Load.loadReportId report @?= LoadId 5
          Load.loadReportPath report @?= path
          Load.loadReportGoals report @?= []
          onlyGoals report
 where
  wrongStep :: Text
  wrongStep = "cong (x +_) (*-distribʳ-+ x y z)"

  step :: Text
  step =
    "trans (cong (x +_) (*-distribʳ-+ x y z)) \
    \(sym (+-assoc x (y * x) (z * x)))"

  elaboratedStep :: Text
  elaboratedStep =
    "trans (cong (x +_) (*-distribʳ-+ x y z))\n\
    \(sym (+-assoc x (y * x) (z * x)))"

  splitProof :: [Text]
  splitProof = [signature, baseClause <> "?", stepClause <> "?"]

  signature :: Text
  signature = "*-distribʳ-+ : _*_ DistributesOverʳ _+_"

  baseClause :: Text
  baseClause = "*-distribʳ-+ x zero z = "

  stepClause :: Text
  stepClause = "*-distribʳ-+ x (suc y) z = "

  proofShouldBe :: FilePath -> [Text] -> IO ()
  proofShouldBe path expected = do
    proof <-
      dropWhile (not . Text.isPrefixOf "*-distribʳ-+ :") . Text.lines
        <$> Text.IO.readFile path
    proof @?= expected

-- Helpers

goals :: Load.LoadReport -> [(InteractionId, GoalShape)]
goals = map (\(g, _) -> (goalId g, goalShape g)) . Load.loadReportGoals

contexts :: Load.LoadReport -> [[Binding]]
contexts = map (map binding . snd) . Load.loadReportGoals

bindings :: GoalReport -> [Binding]
bindings = map binding . goalReportContext

-- The user's name for a binding, the name Agda reified it to, whether each is
-- in scope, and the type.
type Binding = (Text, Text, Bool, Bool, Text)

binding :: ContextEntry -> Binding
binding entry =
  ( contextEntryOriginalName entry
  , contextEntryReifiedName entry
  , contextEntryOriginalInScope entry
  , contextEntryReifiedInScope entry
  , contextEntryType entry
  )

natural :: Text -> Binding
natural name = (name, name, True, True, "ℕ")

onlyGoals :: Load.LoadReport -> IO ()
onlyGoals report = do
  Load.loadReportHiddenMetavariables report @?= []
  Load.loadReportWarnings report @?= []
  Load.loadReportNonFatalErrors report @?= []

errorShouldBe :: FilePath -> Error -> [Text] -> IO ()
errorShouldBe path e expected =
  Text.replace (Text.pack path) "FIXTURE" (errorMessage e)
    @?= Text.intercalate "\n" expected

expectLoadOk :: String -> Load.Response -> IO Load.LoadReport
expectLoadOk _ (Load.ResponseOk report) = pure report
expectLoadOk label other =
  assertFailure $ label <> ": expected ResponseOk, got " <> show other

expectGoalOk :: String -> Goal.Response -> IO GoalReport
expectGoalOk _ (Goal.ResponseOk _ report) = pure report
expectGoalOk label other =
  assertFailure $ label <> ": expected ResponseOk, got " <> show other

expectCheckOk :: String -> Check.Response -> IO Check.CheckReport
expectCheckOk _ (Check.ResponseOk _ report) = pure report
expectCheckOk label other =
  assertFailure $ label <> ": expected ResponseOk, got " <> show other

expectSplitApplied ::
  String -> CaseSplit.Response -> IO (CaseSplit.Edit, Load.LoadReport)
expectSplitApplied
  label
  (CaseSplit.ResponseCompleted _ (CaseSplit.OutcomeApplied edit) reload) =
    (,) edit <$> expectLoadOk (label <> "'s resync") reload
expectSplitApplied label other =
  assertFailure $ label <> ": expected OutcomeApplied, got " <> show other

expectGiveCompleted ::
  String -> Give.Response -> IO (Give.Outcome, Load.LoadReport)
expectGiveCompleted label (Give.ResponseCompleted outcome reload) =
  (,) outcome <$> expectLoadOk (label <> "'s resync") reload
expectGiveCompleted label other =
  assertFailure $ label <> ": expected ResponseCompleted, got " <> show other
