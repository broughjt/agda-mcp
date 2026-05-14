{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.SourceEdit
    ( TextEdit (..)
    , SourceEdit (..)
    , EditError (..)
    , applyTextEditToText
    , applyEditToText
    , applyEditToFile
    ) where

import AgdaMCP.Types (SourceRange)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text.IO qualified as Text.IO
import GHC.Generics (Generic)

-- | A pure text edit with no filesystem path attached.
--
-- This is the main thing we can unit/property test without touching disk.
data TextEdit
    = ReplaceRange SourceRange Text
    | ReplaceWholeLine Int [Text]
    | BatchTextEdits [TextEdit]
    deriving (Eq, Show, Generic)

instance ToJSON TextEdit
instance FromJSON TextEdit

-- | A planned edit to a particular source file.
--
-- Agda response handling should produce this value. The filesystem layer is the
-- only code that should interpret the path and mutate a file.
data SourceEdit = SourceEdit
    { sourceEditFilePath :: FilePath
    , sourceEditTextEdit :: TextEdit
    }
    deriving (Eq, Show, Generic)

instance ToJSON SourceEdit
instance FromJSON SourceEdit

-- | Errors from validating or applying source edits.
data EditError
    = EditNotImplemented Text
    | InvalidRange Text
    | OverlappingEdits Text
    deriving (Eq, Show, Generic)

instance ToJSON EditError
instance FromJSON EditError

-- | Pure edit application.
--
-- Phase 1 only establishes the API. Phase 3 will implement the transformation
-- logic here, and Phase 2 tests can target this function without filesystem IO.
applyTextEditToText :: TextEdit -> Text -> Either EditError Text
applyTextEditToText _edit _input =
    Left $ EditNotImplemented "applyTextEditToText"

-- | Apply a planned source edit to already-loaded file contents.
--
-- The file path is intentionally ignored here; it is only for the filesystem
-- wrapper and for diagnostics.
applyEditToText :: SourceEdit -> Text -> Either EditError Text
applyEditToText edit = applyTextEditToText (sourceEditTextEdit edit)

-- | Filesystem wrapper around 'applyEditToText'.
--
-- This function intentionally delegates all transformation behavior to the pure
-- function above.
applyEditToFile :: SourceEdit -> IO (Either EditError ())
applyEditToFile edit = do
    input <- Text.IO.readFile (sourceEditFilePath edit)
    case applyEditToText edit input of
        Left err -> pure $ Left err
        Right output -> do
            Text.IO.writeFile (sourceEditFilePath edit) output
            pure $ Right ()
