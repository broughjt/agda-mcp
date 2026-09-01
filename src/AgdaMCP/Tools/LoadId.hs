{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.LoadId (
  SessionNonce (..),
  newSessionNonce,
  LoadId (..),
  LoadGeneration (..),
  CurrentLoad (..),
  LoadIdRefusal (..),
  currentLoadId,
  renderLoadId,
  requireCurrentLoad,
  renderLoadIdRefusal,
) where

import Control.Monad (replicateM)
import Data.Aeson (FromJSON (..))
import Data.Aeson.Types qualified as Aeson
import Data.Char (isAsciiLower, isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Text.Read
import Numeric.Natural (Natural)
import System.Random (randomRIO)

import AgdaMCP.Interaction (Hash)

{- | Distinguishes one session's load ids from every other session's.

Load counters start from zero in every process, so without this a load id
issued by a session that has since been replaced would be accepted by its
replacement as soon as the replacement had issued as many loads. The nonce is
four characters from @[a-z0-9]@; the constructor is exported for tests, which
need a fixed one so rendered expectations stay literal.
-}
newtype SessionNonce = SessionNonce Text
  deriving (Eq, Show)

newSessionNonce :: IO SessionNonce
newSessionNonce =
  SessionNonce . Text.pack <$> replicateM nonceLength draw
 where
  draw :: IO Char
  draw = (nonceAlphabet !!) <$> randomRIO (0, length nonceAlphabet - 1)

nonceAlphabet :: String
nonceAlphabet = ['a' .. 'z'] <> ['0' .. '9']

nonceLength :: Int
nonceLength = 4

isNonceChar :: Char -> Bool
isNonceChar c = isAsciiLower c || isDigit c

renderSessionNonce :: SessionNonce -> Text
renderSessionNonce (SessionNonce nonce) = nonce

{- | An identifier which corresponds to the particular current-file state that
produced a set of interaction ids. It names the session as well as the load
within it, so an id from a session that has since been replaced is refused
rather than mistaken for one of the replacement's.
-}
data LoadId = LoadId SessionNonce Natural
  deriving (Eq, Show)

renderLoadId :: LoadId -> Text
renderLoadId (LoadId nonce n) =
  "L" <> Text.pack (show n) <> "-" <> renderSessionNonce nonce

instance FromJSON LoadId where
  parseJSON = Aeson.withText "load_id" $ \text ->
    case Text.stripPrefix "L" text of
      Just digits
        | Right (n, rest) <- Text.Read.decimal digits
        , Just nonce <- Text.stripPrefix "-" rest
        , Text.length nonce == nonceLength
        , Text.all isNonceChar nonce ->
            pure $ LoadId (SessionNonce nonce) n
      _ ->
        fail $
          "expected a load_id from the most recent result, such as \
          \\"L17-k7x2\", but got "
            <> show text

{- | Loading replaces Agda's active interaction state and reuses small
interaction ids, so a goal id only means something against the load that
issued it. This tracks which load that is.
-}
data LoadGeneration = LoadGeneration
  { sessionNonce :: SessionNonce
  , loadsIssued :: Natural
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
  LoadId (sessionNonce generation) (loadsIssued generation)
    <$ currentLoad generation

-- | Why a `LoadId` was refused.
data LoadIdRefusal
  = -- | Nothing is loaded, so no goal id can mean anything yet.
    NoCurrentLoad
  | {- | The submitted id names an earlier load generation. The payload is the
    id that is current, so the caller can tell how far behind it is.
    -}
    StaleLoadId LoadId
  | {- | The submitted id was issued by a session other than this one, so its
    counter says nothing about how far behind it is.
    -}
    ForeignLoadId
  deriving (Eq, Show)

requireCurrentLoad ::
  LoadGeneration -> LoadId -> Either LoadIdRefusal CurrentLoad
requireCurrentLoad generation submitted@(LoadId nonce _) =
  case (,) <$> currentLoadId generation <*> currentLoad generation of
    Nothing -> Left NoCurrentLoad
    Just (current, load)
      | nonce /= sessionNonce generation -> Left ForeignLoadId
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
renderLoadIdRefusal ForeignLoadId =
  "The supplied load ID is from a session that no longer exists. Load the \
  \file again and use the load ID from that result."
