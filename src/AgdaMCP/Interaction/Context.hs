module AgdaMCP.Interaction.Context (
  Request (..),
  Response,
  context,
  extractContext,
) where

import Agda.Interaction.Base (Rewrite)
import Agda.Interaction.BasicOps (getResponseContext)
import Agda.Interaction.Command (CommandM, liftLocalState)
import Agda.Interaction.Response (ResponseContextEntry (..))
import Agda.Syntax.Abstract.Pretty (prettyATop)
import Agda.Syntax.Common (
  Arg (..),
  InteractionId,
  ModalPolarity (..),
  Modality,
  getCohesion,
  getModalPolarity,
  getQuantity,
  getRelevance,
  inverseComposeRelevance,
  isInstance,
  isRelevant,
  modPolarityAnn,
  moreQuantity,
 )
import Agda.Syntax.Common.Pretty (prettyShow, render)
import Agda.Syntax.Concrete.Name (NameInScope (..), isInScope)
import Agda.TypeChecking.Errors (verbalize)
import Agda.TypeChecking.Monad (
  TCM,
  currentModality,
  withInteractionId,
 )
import Control.Monad.State (lift)
import Data.Text qualified as Text

import AgdaMCP.Interaction.Internal (InteractionM, catchTCErr, runCommandM)
import AgdaMCP.Interaction.Model (
  ContextEntry (..),
  GoalError (..),
  classifyInteractionError,
 )

data Request = Request
  { requestNormalization :: Rewrite
  , requestGoalId :: InteractionId
  }
  deriving (Eq, Show)

-- `Left` is a bad id or a `TCErr`, while `Right` is the goal's context, ordered
-- with local variables outermost first then let bindings, following
-- `contextOfMeta`.
type Response = Either GoalError [ContextEntry]

context :: Request -> InteractionM Response
context = runCommandM . contextInternal

contextInternal :: Request -> CommandM Response
contextInternal (Request normalization goalId) =
  -- The meat of `interpret Cmd_context` (InteractionTop.hs:706-707) is
  -- `getResponseContext`. We run the query under `liftLocalState`, the idea
  -- being that display commands should not modify `TCState` (apparently
  -- reification can allocate metas, etc.). The Emacs renderer runs under
  -- `localTCState` for the same reason (EmacsTop.hs:212). A non-existent id is
  -- classified by `classifyInteractionError`; any other `TCErr` becomes
  -- `GoalFailed`.
  (Right <$> liftLocalState (extractContext normalization goalId))
    `catchTCErr` (fmap Left . lift . classifyInteractionError GoalUnknownId GoalFailed)

-- The extraction mirrors `prettyResponseContext` (EmacsTop.hs:324-373). Also
-- see the comment above `ContextEntry`.
extractContext :: Rewrite -> InteractionId -> TCM [ContextEntry]
extractContext normalization goalId = withInteractionId goalId $ do
  entries <- getResponseContext normalization goalId
  goalModality <- currentModality
  traverse (extractContextEntry goalModality) entries

extractContextEntry :: Modality -> ResponseContextEntry -> TCM ContextEntry
extractContextEntry
  goalModality
  ( ResponseContextEntry
      originalName
      reifiedName
      (Arg info ty)
      letValue
      reifiedInScope
    ) = do
    typeDoc <- prettyATop ty
    letValueDoc <- traverse prettyATop letValue
    pure
      ContextEntry
        { contextEntryOriginalName = Text.pack $ prettyShow originalName
        , contextEntryReifiedName = Text.pack $ prettyShow reifiedName
        , contextEntryOriginalInScope = isInScope originalName == InScope
        , contextEntryReifiedInScope = reifiedInScope == InScope
        , contextEntryType = Text.pack $ render typeDoc
        , contextEntryLetValue = Text.pack . render <$> letValueDoc
        , -- Exactly what `prettyResponseContext` does it:
          contextEntryIsInstance = isInstance info
        , contextEntryCohesion = cohesion
        , contextEntryPolarity = polarity
        , -- Exactly what `prettyResponseContext` does it:
          contextEntryErased =
            not $ getQuantity info `moreQuantity` getQuantity goalModality
        , contextEntryRelevance = relevance
        }
   where
    -- Emacs renders cohesion with `prettyShow`. The default `Continuous`
    -- `prettyShow`s as empty (Syntax/Common.hs:1881-1884), which we detect with
    -- `null` and extract as `Nothing`.
    cohesion :: Maybe Text.Text
    cohesion =
      let c = prettyShow $ getCohesion info
       in if null c then Nothing else Just $ Text.pack c

    polarity :: Maybe Text.Text
    polarity =
      -- Exactly as `prettyResposneContext` does it
      let p = modPolarityAnn $ getModalPolarity info
       in if p == MixedPolarity
            then Nothing
            else Just $ Text.pack $ verbalize p

    relevance :: Maybe Text.Text
    relevance =
      -- Exactly as `prettyResposneContext` does it
      let r = getRelevance goalModality `inverseComposeRelevance` getRelevance info
       in if isRelevant r then Nothing else Just $ Text.pack $ verbalize r
