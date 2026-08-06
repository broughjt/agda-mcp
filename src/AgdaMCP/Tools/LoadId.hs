{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.LoadId (
  LoadId (..),
  LoadGeneration (..),
  CurrentLoad (..),
  LoadIdRefusal (..),
  currentLoadId,
  renderLoadId,
  requireCurrentLoad,
  renderLoadIdRefusal,
) where

import Data.Aeson (FromJSON (..))
import Data.Aeson.Types qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Text.Read
import Numeric.Natural (Natural)

import AgdaMCP.Interaction (Hash)

{- | An identifier which corresponds to the particular current-file state that
produced a set of interaction ids.
-}
newtype LoadId = LoadId Natural
  deriving (Eq, Show)

renderLoadId :: LoadId -> Text
renderLoadId (LoadId n) = "L" <> Text.pack (show n)

instance FromJSON LoadId where
  parseJSON = Aeson.withText "load_id" $ \text ->
    case Text.stripPrefix "L" text of
      Just digits
        | Right (n, rest) <- Text.Read.decimal digits
        , Text.null rest ->
            pure $ LoadId n
      _ ->
        fail $
          "expected a load_id from the most recent result, such as \"L17\", \
          \but got "
            <> show text

{- | Loading replaces Agda's active interaction state and reuses small
interaction ids, so a goal id only means something against the load that
issued it. This tracks which load that is.
-}
data LoadGeneration = LoadGeneration
  { loadsIssued :: Natural
  , currentLoad :: Maybe CurrentLoad
  }

data CurrentLoad = CurrentLoad
  { currentLoadPath :: FilePath
  -- ^ Path of the currently loaded file.
  , currentLoadSourceHash :: Hash
  -- ^ The hash of the contents of the currently loaded file.
  }
  deriving (Eq, Show)

-- | The id of the current load, if a load is current.
currentLoadId :: LoadGeneration -> Maybe LoadId
currentLoadId generation =
  LoadId (loadsIssued generation) <$ currentLoad generation

-- | Why a `LoadId` was refused.
data LoadIdRefusal
  = -- | Nothing is loaded, so no goal id can mean anything yet.
    NoCurrentLoad
  | {- | The submitted id names an earlier load generation. The payload is the
    id that is current, so the caller can tell how far behind it is.
    -}
    StaleLoadId LoadId
  deriving (Eq, Show)

requireCurrentLoad ::
  LoadGeneration -> LoadId -> Either LoadIdRefusal CurrentLoad
requireCurrentLoad generation submitted =
  case (,) <$> currentLoadId generation <*> currentLoad generation of
    Nothing -> Left NoCurrentLoad
    Just (current, load)
      | submitted == current -> Right load
      | otherwise -> Left $ StaleLoadId current

renderLoadIdRefusal :: LoadIdRefusal -> Text
renderLoadIdRefusal NoCurrentLoad =
  "No load is current. Either no file has been loaded yet, or the most \
  \recent load failed. Load the file, then use the load ID and goal IDs \
  \from that load result."
renderLoadIdRefusal (StaleLoadId current) =
  "The supplied load ID is from an earlier load. The current load ID is "
    <> renderLoadId current
    <> ". Each load makes fresh goal ID assignments, so use the most recently \
       \issued load ID and goal IDs. If you no longer have them, load the file \
       \again."
