{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Types
    ( SourcePosition (..)
    , SourceRange (..)
    , GoalInfo (..)
    , LoadResult (..)
    , GiveResult (..)
    , AgdaError (..)
    , renderAgdaError
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A 1-indexed source position, matching Agda's line/column convention.
data SourcePosition = SourcePosition
    { sourcePositionLine :: Int
    , sourcePositionColumn :: Int
    }
    deriving (Eq, Show, Generic)

instance ToJSON SourcePosition
instance FromJSON SourcePosition

-- | A source range with inclusive-ish Agda positions.
--
-- The exact interpretation will be pinned down when the edit engine is
-- implemented against real Agda ranges. For now this is the boundary type used
-- between Agda response handling and source editing.
data SourceRange = SourceRange
    { sourceRangeStart :: SourcePosition
    , sourceRangeEnd :: SourcePosition
    }
    deriving (Eq, Show, Generic)

instance ToJSON SourceRange
instance FromJSON SourceRange

-- | Information about an open Agda interaction point/hole.
data GoalInfo = GoalInfo
    { goalInfoId :: Int
    , goalInfoType :: Text
    , goalInfoRange :: SourceRange
    }
    deriving (Eq, Show, Generic)

instance ToJSON GoalInfo
instance FromJSON GoalInfo

-- | Result of loading/type-checking an Agda file.
data LoadResult = LoadResult
    { loadResultFilePath :: FilePath
    , loadResultGoals :: [GoalInfo]
    , loadResultErrors :: [Text]
    , loadResultWarnings :: [Text]
    }
    deriving (Eq, Show, Generic)

instance ToJSON LoadResult
instance FromJSON LoadResult

-- | Result of giving an expression to a goal.
--
-- The resulting load state is included because edit-applying tools should
-- reload and report the new Agda state after mutating the source file.
data GiveResult = GiveResult
    { giveResultFilePath :: FilePath
    , giveResultExpression :: Text
    , giveResultReload :: LoadResult
    }
    deriving (Eq, Show, Generic)

instance ToJSON GiveResult
instance FromJSON GiveResult

-- | Errors produced by our Agda runtime layer.
data AgdaError
    = AgdaNotImplemented Text
    | AgdaRuntimeStopped
    | AgdaRuntimeError Text
    | AgdaProtocolError Text
    | AgdaEditError Text
    deriving (Eq, Show, Generic)

instance ToJSON AgdaError
instance FromJSON AgdaError

renderAgdaError :: AgdaError -> Text
renderAgdaError (AgdaNotImplemented msg) = "Not implemented yet: " <> msg
renderAgdaError AgdaRuntimeStopped = "Agda runtime has been stopped"
renderAgdaError (AgdaRuntimeError msg) = "Agda runtime error: " <> msg
renderAgdaError (AgdaProtocolError msg) = "Agda protocol error: " <> msg
renderAgdaError (AgdaEditError msg) = "Source edit error: " <> msg
