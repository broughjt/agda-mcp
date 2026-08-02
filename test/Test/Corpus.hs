{-# LANGUAGE OverloadedStrings #-}

-- Payloads observed from the fixtures rather than invented. `Test.Interaction`
-- pins each one against Agda, while `Test.Tool.Load` renders it.
module Test.Corpus (
  fixtureDirectory,
  fixtureFile,
  normalizeWarning,
  normalizeNonFatalError,
  normalizeError,
  unreachableClauseWarnings,
  typeError,
  warningThenError,
  safeFlagPostulate,
  unsolvedConstraints,
  constrainedGoal,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath (takeDirectory, (</>))

import AgdaMCP.Interaction (
  Error (..),
  Goal (..),
  GoalShape (..),
  NonFatalError (..),
  Position (..),
  Span (..),
  Warning (..),
 )

-- The staging directory every payload below is written against.
fixtureDirectory :: FilePath
fixtureDirectory = "/fixture"

fixtureFile :: FilePath -> FilePath
fixtureFile = (fixtureDirectory </>)

-- Fixtures are staged into a fresh temporary directory, so the path Agda embeds
-- in a message differs on every run. The argument is the staged fixture path.
normalizeWarning :: FilePath -> Warning -> Warning
normalizeWarning staged (Warning (location, text)) =
  Warning (normalizeLocation staged location, normalizeText staged text)

normalizeNonFatalError :: FilePath -> NonFatalError -> NonFatalError
normalizeNonFatalError staged (NonFatalError (location, text)) =
  NonFatalError (normalizeLocation staged location, normalizeText staged text)

normalizeError :: FilePath -> Error -> Error
normalizeError staged e =
  Error
    { errorMessage = normalizeText staged (errorMessage e)
    , errorPathSpan = normalizeLocation staged (errorPathSpan e)
    , errorWarnings = map (normalizeWarning staged) (errorWarnings e)
    }

normalizeLocation ::
  FilePath -> Maybe (FilePath, Span) -> Maybe (FilePath, Span)
normalizeLocation staged = fmap (\(path, s) -> (normalizePath staged path, s))

normalizePath :: FilePath -> FilePath -> FilePath
normalizePath staged = Text.unpack . normalizeText staged . Text.pack

normalizeText :: FilePath -> Text -> Text
normalizeText staged =
  Text.replace
    (Text.pack $ takeDirectory staged)
    (Text.pack fixtureDirectory)

-- Warnings.agda
unreachableClauseWarnings :: [Warning]
unreachableClauseWarnings =
  [ Warning
      ( Just
          ( fixtureFile "Warnings.agda"
          , Span (Position 116 8 1) (Position 127 8 12)
          )
      , message
          [ "/fixture/Warnings.agda:8.1-12: warning: -W[no]UnreachableClauses"
          , "Unreachable clause"
          , "when checking the definition of first"
          ]
      )
  , Warning
      ( Just
          ( fixtureFile "Warnings.agda"
          , Span (Position 182 13 1) (Position 194 13 13)
          )
      , message
          [ "/fixture/Warnings.agda:13.1-13: warning: -W[no]UnreachableClauses"
          , "Unreachable clause"
          , "when checking the definition of second"
          ]
      )
  ]

-- TypeError.agda
typeError :: Error
typeError =
  Error
    { errorMessage =
        message
          [ "/fixture/TypeError.agda:6.9-10: error: [UnequalTerms]"
          , "Set !=< ℕ"
          , "when checking that the expression ℕ has type ℕ"
          ]
    , errorPathSpan =
        Just
          ( fixtureFile "TypeError.agda"
          , Span (Position 76 6 9) (Position 77 6 10)
          )
    , errorWarnings = []
    }

-- WarningThenError.agda
warningThenError :: Error
warningThenError =
  Error
    { errorMessage =
        message
          [ "/fixture/WarningThenError.agda:11.9-10: error: [UnequalTerms]"
          , "Set !=< ℕ"
          , "when checking that the expression ℕ has type ℕ"
          ]
    , errorPathSpan =
        Just
          ( fixtureFile "WarningThenError.agda"
          , Span (Position 157 11 9) (Position 158 11 10)
          )
    , errorWarnings =
        [ Warning
            ( Just
                ( fixtureFile "WarningThenError.agda"
                , Span (Position 124 8 1) (Position 135 8 12)
                )
            , message
                [ "/fixture/WarningThenError.agda:8.1-12: warning: -W[no]UnreachableClauses"
                , "Unreachable clause"
                , "when checking the definition of first"
                ]
            )
        ]
    }

-- SafePostulate.agda loaded with --safe
safeFlagPostulate :: NonFatalError
safeFlagPostulate =
  NonFatalError
    ( Just
        ( fixtureFile "SafePostulate.agda"
        , Span (Position 40 4 3) (Position 61 4 24)
        )
    , message
        [ "/fixture/SafePostulate.agda:4.3-24: error: [SafeFlagPostulate]"
        , "Cannot postulate cheat with safe flag"
        , "when scope checking the declaration"
        , "  cheat : {A : Set} → A"
        ]
    )

-- Constrained.agda
unsolvedConstraints :: NonFatalError
unsolvedConstraints =
  NonFatalError
    ( Nothing
    , message
        [ "error: [UnsolvedConstraints]"
        , "Failed to solve the following constraints:"
        , "  ?0 + ?0 = 4 : ℕ (blocked on _n_4)"
        ]
    )

constrainedGoal :: Goal
constrainedGoal =
  Goal
    { goalId = 0
    , goalSpan = Span (Position 291 12 17) (Position 292 12 18)
    , goalShape = GoalOfType "ℕ"
    }

-- Helpers

message :: [Text] -> Text
message = Text.intercalate "\n"
