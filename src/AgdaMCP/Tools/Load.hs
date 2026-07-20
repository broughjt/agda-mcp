{-# LANGUAGE OverloadedStrings #-}

module AgdaMCP.Tools.Load (
  loadTool,
) where

import Data.Aeson (object, (.=))
import Data.Map qualified as Map
import Data.Text (Text)
import MCP.Server (
  InputSchema (..),
  ToolHandler,
  toolHandler,
 )

loadTool :: ToolHandler
loadTool =
  toolHandler
    "load"
    ( Just
        "Load and typecheck an Agda source file. Reports open goals (each with \
        \the local context at the goal), unsolved hidden metavariables, \
        \non-fatal errors, and warnings on success, or the Agda error if \
        \checking fails. Relative paths are resolved against the server \
        \process's working directory; prefer an absolute path when that \
        \directory may be ambiguous."
    )
    ( InputSchema
        "object"
        ( Just $
            Map.fromList
              [
                ( "path"
                , object
                    [ "type" .= ("string" :: Text)
                    , "description"
                        .= ( "Path to an Agda source file (.agda, but also \
                             \literate formats such as .lagda.md, .lagda.tex, \
                             \.lagda.typ, etc). Relative paths are resolved \
                             \against the server process's working directory." ::
                               Text
                           )
                    ]
                )
              ]
        )
        (Just ["path"])
    )
    -- TODO:
    (error "un")
