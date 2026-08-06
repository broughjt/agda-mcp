module AgdaMCP.Interaction.MakeCase (
  Request (..),
  Split (..),
  Response,
  MakeCaseReport (..),
  MakeCaseVariant (..),
  toMakeCaseVariant,
  MakeCaseError (..),
  makeCase,
) where

import Agda.Interaction.Command (CommandM)
import Agda.Interaction.InteractionTop (
  decorate,
  extlam_dropName,
  liftCommandMT,
 )
import Agda.Interaction.MakeCase (CaseContext)
import Agda.Interaction.MakeCase qualified as MakeCase
import Agda.Interaction.Options (getPragmaOptions, optUseUnicode)
import Agda.Syntax.Abstract qualified as A
import Agda.Syntax.Abstract.Name (qnameModule)
import Agda.Syntax.Abstract.Pretty (prettyAUnqualify)
import Agda.Syntax.Common (InteractionId)
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.Syntax.Position (fuseRange)
import Agda.TypeChecking.Monad (
  IPClause (..),
  InteractionPoint (..),
  TCM,
  addContext,
  getsTC,
  inTopContext,
  lookupSection,
  withInteractionId,
 )
import Agda.TypeChecking.Monad.MetaVars (
  getInteractionRange,
  lookupInteractionPoint,
 )
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (lift)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text

import AgdaMCP.Interaction.Extract (classifyInteractionError, rangeSpan)
import AgdaMCP.Interaction.Internal (InteractionM, catchTCErr, runCommandM)
import AgdaMCP.Interaction.Model (
  Error,
  Span,
 )

data Request = Request
  { requestGoalId :: InteractionId
  , requestSplit :: Split
  }
  deriving (Eq, Show)

{- | Which make-case action to perform at the goal.

The `makeCase` helper (MakeCase.hs:249-467) splits a string on three cases by
running it through `words`: a non-empty list of variables, a single period, or
an empty string. We model these three cases as a sum type instead of using the
string.
-}
data Split
  = {- | Split on one or more pattern variables, identified by name
    (MakeCase.hs:375-436).

    A name that is not currently in scope is not split on but brought into scope
    as a visible pattern (the @toShow@/@toSplit@ partition,
    MakeCase.hs:381-382), so this constructor covers both case splitting and
    revealing a hidden arguments.
    -}
    SplitVariables (NonEmpty Text)
  | {- | Introduces a new arguments if possible, stopping if any are visible and
    splitting into copattern clauses otherwise. (MakeCase.hs:325-374).

    Agda first inserts remaining arguments, and if any of them is "visible" it
    stops there and returns the clause with the new arguments rather than
    splitting (issues #1516/#2654, :332-342). Actual result splitting occurs if
    the argument is record-typed and @--copatterns@ is on (it is by default,
    Options/Types.hs:129).
    -}
    IntroduceArgumentsOrSplitResult
  | {- | No idea (MakeCase.hs:319-321).

    I can't tell what this is used for. I'm preserving this in attempt to write
    a faithful wrapper, but I'm honestly pretty lost about what this does. It
    has something to do with expanding the leading `...` ellipses in with
    clauses.
    -}
    ExpandEllipsis
  deriving (Eq, Show)

type Response = Either MakeCaseError MakeCaseReport

data MakeCaseReport = MakeCaseReport
  { makeCaseReportVariant :: MakeCaseVariant
  -- ^ How the clauses below are meant to be laid out.
  , makeCaseReportClauses :: [Text]
  {- ^ The replacement clauses, one per element. Each is rendered in
  @OneLineMode@ (@decorate@, InteractionTop.hs:795-796), so no element ever
  contains a newline however long it is.
  -}
  , makeCaseReportSpan :: Span
  {- ^ The part of the file the clauses are intended to replace, beginning at
  the start of the clause's left-hand side to the end of its right-hand side,
  excluding any where block.

  For example, for this clause:

  @
  double :: ℕ → ℕ
  double n = ?
  @

  the span covers exactly:

  @
  double n = ?
  @

  After splitting on @n@, the span is replaced with:

  @
  double zero = ?
  double (suc n) = ?
  @

  The @Resp_MakeCase@ constructor does not carry a span, so the Emacs frontend
  guesses one. It deletes from the current line's indentation to the end of the
  line (@agda2-make-case-action@, agda2-mode.el:916-928) or matches backwards
  from the hole for an extended lambda (@agda2-make-case-action-extendlam@,
  agda2-mode.el:930-953). We diverge from Emacs here by obtaining the needed
  span from the interaction point state.

  Note that the @where@ block is deliberately excluded, even though @getRange@
  of a whole @Clause'@ would include it (Abstract.hs:738-739). The replacement
  clauses are built by @makeAbstractClause@ (MakeCase.hs:525-528), which reuses
  the split clause's right-hand side but passes @A.noWhereDecls@.
  -}
  , makeCaseReportCollapsesWhere :: Bool
  {- ^ True exactly when replacing @makeCaseReportSpan@ leaves a @where@ block
  attached to the last generated clause alone.

  The split clause carried a @where@ block, which the span above deliberately
  excludes. Layout binds a @where@ block to the clause it follows, so after the
  replacement the block belongs to the last generated clause and the earlier
  ones can no longer see its bindings. Splitting on @n@ in:

  @
  withWhere : ℕ → ℕ
  withWhere n = ? + helper
    where
      helper : ℕ
      helper = zero
  @

  produces:

  @
  withWhere : ℕ → ℕ
  withWhere zero = ? + helper
  withWhere (suc n) = ? + helper
    where
      helper : ℕ
      helper = zero
  @

  in which @helper@ is out of scope in the first clause, though it was in scope
  for the clause that was split. The right-hand side is reused whole for every
  generated clause, so the caller wrote nothing wrong and yet the file no longer
  typechecks. Emacs behaves identically, so this is reported rather than fixed.
  -}
  }
  deriving (Eq, Show)

{- | Mirrors Agda's @MakeCaseVariant@. We reproduce this because Agda's has no
`Eq`/`Show` instances (same with @GiveAction@).
-}
data MakeCaseVariant
  = -- | An ordinary function definition, whose clauses are written one per line.
    MakeCaseFunction
  | {- | A pattern-matching lambda, whose clauses are written inside @λ { ... }@
    separated by @;@.
    -}
    MakeCaseExtendedLambda
  deriving (Eq, Show)

-- | Exactly the same as @makeCaseVariant@ (InteractionTop.hs:798-800).
toMakeCaseVariant :: CaseContext -> MakeCaseVariant
toMakeCaseVariant Nothing = MakeCaseFunction
toMakeCaseVariant Just {} = MakeCaseExtendedLambda

data MakeCaseError
  = MakeCaseUnknownId InteractionId
  | MakeCaseFailed Error
  deriving (Eq, Show)

makeCase :: Request -> InteractionM Response
makeCase = runCommandM . makeCaseInternal

makeCaseInternal :: Request -> CommandM Response
makeCaseInternal (Request goalId split) =
  ( do
      -- Copied from @interpret (Cmd_make_case ii rng s)@
      -- (InteractionTop.hs:759-780) with the @putResponse@ replaced by a
      -- return.

      -- Emacs does not pass the range to @makeCase@ in the common case, when
      -- the variable to split on is obtained by reading from the minibuffer. We
      -- always pass it anyway, since the only place it is used is
      -- @parseVariables@, which updates the metavariable's range
      -- (@updateMetaVarRange@, MakeCase.hs:76).
      range <- lift $ getInteractionRange goalId
      (name, caseContext, clauses) <-
        lift $ MakeCase.makeCase goalId range (splitInput split)
      liftCommandMT (withInteractionId goalId) $ do
        telescope <- lift $ lookupSection (qnameModule name)
        unicode <- getsTC $ optUseUnicode . getPragmaOptions
        documents <-
          lift $ inTopContext $ addContext telescope $ traverse prettyAUnqualify clauses
        (span', collapsesWhere) <- lift $ extractClauseSpan goalId
        pure $
          Right
            MakeCaseReport
              { makeCaseReportVariant = toMakeCaseVariant caseContext
              , makeCaseReportClauses =
                  map
                    (Text.pack . extlam_dropName unicode caseContext . decorate)
                    documents
              , makeCaseReportSpan = span'
              , makeCaseReportCollapsesWhere = collapsesWhere
              }
  )
    `catchTCErr` (fmap Left . lift . classifyInteractionError MakeCaseUnknownId MakeCaseFailed)

-- Re-encode the split as the string Agda parses with `words`.
splitInput :: Split -> String
splitInput (SplitVariables variables) =
  Text.unpack $ Text.unwords $ toList variables
splitInput IntroduceArgumentsOrSplitResult = ""
splitInput ExpandEllipsis = "."

{- | The span the replacement clauses are meant to replace, and whether the
clause being replaced carries a @where@ block (see
`makeCaseReportCollapsesWhere`).
-}
extractClauseSpan :: InteractionId -> TCM (Span, Bool)
extractClauseSpan goalId = do
  -- Both of the failure modes here are bugs, since we execute this after
  -- @makeCase@ has succeeded. A goal that is not in a clause fails inside
  -- @makeCase@ with a @CaseSplitError@ ("Cannot split here, as we are not in a
  -- function declaration", MakeCase.hs:256-257).
  point <- lookupInteractionPoint goalId
  case ipClause point of
    IPNoClause -> liftIO $ throwIO $ MakeCaseNoClause goalId
    IPClause {ipcClause = clause} -> do
      let range = fuseRange (A.clauseLHS clause) (A.clauseRHS clause)
          collapsesWhere = isJust $ A.whereDecls $ A.clauseWhereDecls clause
      maybe
        (liftIO $ throwIO $ MakeCaseClauseNoRange goalId $ Text.pack $ prettyShow range)
        (pure . (,collapsesWhere))
        (rangeSpan range)

data MakeCaseBug
  = -- | The goal turned out not to sit in a clause after a split succeeded.
    MakeCaseNoClause InteractionId
  | -- | The split clause has no source range, so we cannot say what to replace.
    MakeCaseClauseNoRange InteractionId Text
  deriving (Show)

instance Exception MakeCaseBug
