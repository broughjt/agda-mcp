# The interaction layer

Why agda-mcp drives Agda through its own layer of typed command wrappers
(`interaction/AgdaMCP/Interaction/*`) rather than through Agda's interaction
layer, and what that layer has to do to be correct.

This is a design record for contributors and for future me. It is not user
documentation. It assumes you can read Haskell and are willing to open Agda's
source alongside it.

**Citations.** Paths like `Interaction/InteractionTop.hs:252` are relative to
`src/full/Agda/` in the Agda source tree. Paths like
`interaction/AgdaMCP/Interaction/Load.hs:107` are relative to this repository.
Agda citations were verified against commit `3d04bacc` (2025-07-05), whose
`Agda.cabal` reports `version: 2.8.0` but which is post-release master rather
than the released tarball — the two have already been observed to differ (a
rendering helper is `showGoals` there and `prettyGoals` in the release). Line
numbers drift. When one does not resolve, find the definition by name; the
names have been much more stable than the lines. The wrapper suite
(`Test.Interaction`) is the executable version of everything claimed here about
Agda's behaviour, so when this document and the suite disagree, the suite wins.

**Scope.** This document owns everything that determines the shape of the
boundary between agda-mcp and Agda. The tool layer above it (`src/AgdaMCP/*`)
appears only as assumptions the boundary relies on; its own design — `load_id`
and staleness policy, rendering, MCP schemas — belongs in a separate document.

---

## 1. How frontends interact with Agda

### 1.1 One loop, two serializers

Agda's interactive mode is a line-oriented protocol over stdin/stdout. `Main.hs`
dispatches `--interaction` to `mimicGHCi` (`Interaction/EmacsTop.hs:52-53`) and
`--interaction-json` to `jsonREPL` (`Interaction/JSONTop.hs`). The name
`mimicGHCi` is historical: the protocol pretends to be a GHCi session, prompt
and all.

Both are one-liners over the same loop:

```haskell
mimicGHCi = repl (liftIO . mapM_ (putStrLn . prettyShow) <=< lispifyResponse) "Agda2> "
```

`repl` (`Interaction/AgdaTop.hs:29`) sets line buffering and UTF-8 on both
handles, installs the serializer with `setInteractionOutputCallback`, starts a
command queue, and loops on `maybeAbort runInteraction`. The Emacs frontend and
the JSON frontend differ *only* in the callback they pass. Everything from
`InteractionTop` inward is frontend-agnostic.

That fact is what makes this project possible at all: there is a seam, and both
existing frontends sit on it. Most of this document is about why that seam is in
the wrong place for us, and where we sit instead.

### 1.2 Inbound: the IOTCM

A client sends one command per line, in the concrete syntax of Haskell's derived
`Show`. Agda parses it with `reads` (`parseIOTCM`, `Interaction/Base.hs:352`):

```
IOTCM "/path/to/File.agda" Interactive Indirect ( Cmd_goal_type_context Simplified 0 noRange "" )
```

The pieces are the fields of `IOTCM' range` (`Interaction/Base.hs:322`): the
current file, a `HighlightingLevel`, a `HighlightingMethod`, and an
`Interaction'` constructor. `Interaction'` is the complete command vocabulary —
`Cmd_load`, `Cmd_give`, `Cmd_make_case`, `Cmd_goal_type_context`, and about
fifty others. Because the type derives `Read`, adding a command to the protocol
is just adding a constructor. There is no hand-written wire format inbound.

One wrinkle matters later. The type is

```haskell
type IOTCM = Maybe TopLevelModuleName -> IOTCM' Range
```

(`Interaction/Base.hs:119`) — a *function*, not a value. Ranges sent by a client
carry a file path but cannot carry a `TopLevelModuleName`, because the client
does not know it. `parseIOTCM` therefore returns a function that patches the
module name into every range once the caller supplies it
(`Interaction/Base.hs:353-357`):

```haskell
IOTCM f l m i -> IOTCM f l m $
  (fmap . fmap . fmap) (\rf -> mkRangeFile (rangeFilePath rf) top) i
```

This machinery exists to repair ranges that were deserialized from text. Nothing
else.

### 1.3 Outbound: commands have no return value

Here is the crux. The dispatcher is

```haskell
interpret :: Interaction -> CommandM ()
```

(`Interaction/InteractionTop.hs:477`). Every command returns `()`. Results do
not come back to the caller; anywhere inside the type checker, code calls the
registered `InteractionOutputCallback` with a `Response`
(`Interaction/Response/Base.hs`), and the callback serializes it and prints it.

The response stream is not unary and is not framed. A single command produces a
variable-length sequence of responses, and the only marker that the command has
finished is the reappearance of the `Agda2> ` prompt.

For its intended purpose this is a reasonable design, and it is worth saying so
before criticizing it. Type-checking a large module takes tens of seconds, and a
push channel lets the checker stream highlighting and progress as each
definition is checked instead of leaving the editor blank. The Emacs side is
also nicer than it sounds: responses are elisp function calls, but
`agda2-exec-response` (`src/data/emacs-mode/agda2-mode.el:798`) treats them
as data — the head symbol must carry an `agda2-safe-function` property and every
argument is type-checked with `cl-typep` before the call is applied, so a
confused Agda process cannot make Emacs evaluate arbitrary code. Roughly twenty
whitelisted handlers, type-correct arguments only.

### 1.4 A worked example: one `Cmd_load`

The client sends:

```
IOTCM "/path/to/Data/Sigma/Properties/Equivalence.lagda.typ" None Direct (Cmd_load "/path/to/Data/Sigma/Properties/Equivalence.lagda.typ" [])
```

Agda replies with the following, over the course of the load. Each response
actually occupies a single line; they are shown here prettified, and the JSON
frontend's encoding is used because it is easier to read than the elisp one.

```json
{"kind": "Status",
 "status": {"checked": false, "showImplicitArguments": false, "showIrrelevantArguments": false}}
{"kind": "ClearRunningInfo"}
{"kind": "ClearHighlighting", "tokenBased": "NotOnlyTokenBased"}
{"kind": "RunningInfo", "debugLevel": 1,
 "message": "Checking Data.Sigma.Properties.Equivalence (/path/to/...).\n"}
{"kind": "Status",
 "status": {"checked": false, "showImplicitArguments": false, "showIrrelevantArguments": false}}
{"kind": "DisplayInfo",
 "info": {"kind": "AllGoalsWarnings",
          "errors": [], "warnings": [], "invisibleGoals": [],
          "visibleGoals": [
            {"kind": "OfType",
             "type": "(curry ∘ uncurry') f ＝ identity f",
             "constraintObj": {"id": 0, "range": [{"start": {"line": 691, "col": 26, "pos": 22756},
                                                   "end":   {"line": 691, "col": 30, "pos": 22760}}]}}]}}
{"kind": "InteractionPoints",
 "interactionPoints": [{"id": 0, "range": [{"start": {"line": 691, "col": 26, "pos": 22756},
                                            "end":   {"line": 691, "col": 30, "pos": 22760}}]}]}
```

Seven responses for one command, of which two carry information a client
actually wants. The rest are instructions to an editor: clear the running-info
buffer, drop the highlighting, update the mode line.

Note what a consumer has to know to read this. That the prelude is emitted by
`cmd_load'` before it does anything. That `RunningInfo` may appear zero or many
times depending on verbosity. That `AllGoalsWarnings` arrives because
`interpret (Cmd_load ...)` re-enters `interpret Cmd_metas` as its continuation
(`Interaction/InteractionTop.hs:479-482`, `:508`). That `InteractionPoints` is
emitted not by either of those but by `runInteraction` afterwards, and only if
the command is in the `updateInteractionPointsAfter` table
(`Interaction/InteractionTop.hs:424-466`) *and* the file is still current. None
of this is written down. All of it is load-bearing if you are parsing the
stream.

---

## 2. Two design decisions

### 2.1 Results are instructions to a stateful editor

`interpret` returns `()`, so there is no request/response function anywhere in
the codebase — not over the pipe, and not in-process either. The `Response`
vocabulary is a set of imperatives: `Resp_ClearHighlighting`,
`Resp_JumpToError`, `Resp_DisplayInfo`, `Resp_Status`. Each names an action for
a client with a buffer, a cursor, a mode line, and an info window to perform.

This would be survivable on its own. A consumer could ignore the verbs and read
the payloads. What is not survivable is that *unary queries were folded into the
same channel*. "What is the type of goal 3" has one answer and no reason to be
streamed, but `Cmd_goal_type_context` still returns `()` and still emits its
answer as an instruction to fill an info buffer. There is no layer at which
asking Agda a question yields an answer.

An MCP server needs exactly that. Each tool call is a unary request that must
produce one textual result. Nothing in Agda's interaction layer has that shape,
so any consumer has to rebuild it: buffer the stream, know when it ends,
correlate the pieces, and discard the imperatives.

### 2.2 Extraction is not separated from rendering

Serialization runs in `TCM`. `lispifyResponse :: Response -> TCM [Lisp String]`
(`Interaction/EmacsTop.hs:57`), and `JSONTop.hs` is the same shape. Rendering
code therefore has unrestricted access to the type-checking state, and nothing
prevents it from *fetching more information while rendering*. That access is
used, widely. Three examples, in increasing order of severity:

**The response omits the answer.** `Goal_GoalType`
(`Interaction/Response/Base.hs:118`) is the payload of the
`Cmd_goal_type_context` family:

```haskell
Goal_GoalType Rewrite GoalTypeAux [ResponseContextEntry] [IPFace' Expr] [OutputForm_boot tcErr Expr Expr]
```

Normalization mode, an auxiliary, the context, the boundary faces, the
constraints — and no goal type. Both frontends re-query it at render time by
calling `prettyTypeOfMeta norm ii` (`Interaction/EmacsTop.hs:227`,
`Interaction/JSONTop.hs:395`), which goes back to `typeOfMeta`. The single most
important field of the response is not in the response.

**The response carries its own environment.** `Info_NormalForm`
(`Interaction/Response/Base.hs:109`):

```haskell
Info_NormalForm CommandState ComputeMode (Maybe CPUTime) A.Expr
```

The response ships an entire `CommandState` so that the renderer can re-enter it
via `localStateCommandM` (`Interaction/EmacsTop.hs:150-157`). This is not a data
type with a leak; it is a closure with its environment packed into the payload.

**The response is a handle to state.** `Info_Error` carries a `TCErr`, and
rendering one means `prettyError` (`TypeChecking/Errors.hs:99-100`), which
prints in the state captured inside the error. You cannot turn a `TCErr` into
text without a `TCM` to do it in.

A corroborating symptom: the two frontends *diverge* on shared payloads. For the
constraints in `Goal_GoalType`, Emacs renders each with `prettyTCM` and JSON
with `pretty`, which do not always agree. If the `Response` type were a real
data boundary the rendering decision would have been made once, below it.
Instead each frontend makes it again, and they made it differently.

### 2.3 Consequence: `Response` is not a data boundary

It looks like one. It is a sum type with typed payloads, it sits between the
type checker and two independent serializers, and it is the obvious place to
attach a third. But it does not carry its own meaning. It is a rendering
continuation with some of the arguments prepacked, and the rest fetched from
ambient state when the continuation runs.

The practical consequence is that **the seam both frontends sit on cannot be
consumed as data**, in-process or otherwise. You can attach a third serializer
there, as long as it is willing to run in `TCM` and re-query whatever it needs.
What you cannot do is receive Agda's answers as values.

That the existence of two frontends seems to prove otherwise is a trap. It
proves the seam supports two serializers. Both of them live in `TCM`.

---

## 3. Four attempts

I arrived at the current design by exhausting the alternatives. Each attempt
failed for a different reason, and the reasons are the argument for what
replaced them.

### Attempt 1: subprocess, in Rust

Spawn `agda --interaction-json`, write IOTCMs to its stdin, parse the JSON
responses.

This foundered on two things. The first was the response grammar. A schema for
individual responses is not enough — you need to know which *sequences* are
possible, which is the knowledge §1.4 sketches and which exists nowhere but in
the source. I was reverse-engineering it while also not yet understanding the
interaction layer at all well.

The second was worse. Consuming Agda's output in another language means
reimplementing Agda's model in that language: goal context entries, spans,
normalization modes, the distinction between visible and hidden metavariables.
Every one of those is a small pile of decisions Agda has already made, and
getting one wrong produces a plausible-looking wrong answer rather than a type
error. This is flaw 2.1 charged at full price: because commands have no return
value, there is no way to get Agda's own data structures, so you rebuild them
from rendered text.

### Attempt 2: in-process, worker thread, channel

Same protocol, no subprocess: run Agda's loop on a Haskell thread and receive
`Response` values on a channel. Now at least the types are Agda's own.

This failed on flaw 2.2, in the purest form it takes. Responses do not carry
what needs to be rendered, and rendering requires `TCM`, which lives on the
other thread. So the data cannot come to the code — the code has to go to the
data. I ended up **sending a rendering callback across the channel** to be run
in `TCM` on the worker thread, and receiving the rendered text back.

It worked. It was also obviously wrong: the "boundary" between my code and Agda
now had functions crossing it in both directions, and any invariant about what
runs where had become impossible to state.

### Attempt 3: the session is a value

Then I noticed that `TCM` is not really a state monad. It is a reader over an
`IORef`:

```haskell
newtype TCMT m a = TCM { unTCM :: IORef TCState -> TCEnv -> m a }
```

(`TypeChecking/Monad/Base.hs:6031`), and `runTCM` allocates a *fresh* ref per
call and returns the final state alongside the result
(`TypeChecking/Monad/Base.hs:6291`). So `TCState` can be held as an ordinary
value between commands. Together with `CommandState`, that is the entire
session. No thread, no channel, no locks: keep the pair in a state monad of my
own and enter `runTCM` when a command needs running.

This is the load-bearing realization, and it is not signposted anywhere. Agda's
own architecture — reader thread, command queue, callbacks — actively suggests
the opposite. §4 goes into what it buys.

It worked, and it was a real improvement. But it did not fix anything about
where information comes from. I was still parsing response shapes, and I still
needed helpers that run in `TCM` outside of any Agda command. At commit
`2970483` the load *tool* contained a `contextOf` helper that called
`getResponseContext` directly. That is tool business logic reaching into the
type checker.

What bothered me was not that this was slow or ugly. It was that there was no
line anywhere in the program on one side of which Agda's state could change.
`TCM`-capable code was scattered across tool modules, and every one of them was
somewhere I had to think about Agda's invariants. The obligation was open-ended:
any code anyone added later could mutate the session.

### Attempt 4: call Agda's utilities directly from tools

The other extreme: drop the response stream entirely, call `BasicOps` and
friends from tool implementations, query `TCState` for whatever the tool needs.

I abandoned this quickly, after reading `give_gen`. Agda's command
implementations are intricate, they encode a great deal of accumulated
correctness, and they change between versions. Reimplementing or inlining them
means owning that drift forever, and the drift is silent — a give that stops
handling some case correctly does not announce itself.

And the invariant problem from attempt 3 was not fixed by this, it was made
worse: now *every* tool would be maintaining Agda state directly.

### What the attempts establish

Attempts 3 and 4 both worked, in the sense that they produced running code. They
failed for the same reason, and it is not a complaint about coupling.

**Coupling to Agda's internals is unavoidable for this program.** There is no
supported API; there is a seam that cannot be consumed as data and a pile of
internal utilities. Any design is coupled. The question is not how to avoid the
coupling but *where it lives and how it is checked*.

So the goal is to turn an open-ended obligation into a closed one. Instead of
"anyone touching tool code must maintain Agda's invariants," the rule becomes:
Agda's invariants are maintained in sixteen modules, one per Agda command, each
of which cites the Agda code it mirrors, and nowhere else. That is what the
interaction layer is.

Attempts 3 and 4 also pin down two requirements that pull against each other,
and the wrapper is where they meet:

- **Do not reimplement Agda's commands** (attempt 4). A wrapper must call
  `cmd_load'`, `give_gen`, `MakeCase.makeCase` — which means running inside
  `CommandM`.
- **Do not let `TCM` escape into tool code** (attempt 3). A wrapper must return
  plain data, with no residual dependency on the type-checking monad.

§6 is about how a wrapper discharges both.

---

## 4. The session is a value

### 4.1 The state

```haskell
data InteractionState = InteractionState TCState CommandState GiveSlot

type InteractionM = StateT InteractionState IO
```

(`interaction/AgdaMCP/Interaction/Internal.hs:49-54`). The constructor is not
exported. Callers of the sublibrary get `InteractionState` as an opaque token
and can do nothing with it but pass it to a wrapper, which is the mechanical
enforcement of "wrappers are the only mutators of Agda state."

`GiveSlot` is an `IORef` used by exactly one mechanism; see §6.5.

### 4.2 Entering Agda

```haskell
runCommandM :: CommandM a -> InteractionM a
runCommandM action =
  StateT $ \(InteractionState tcState commandState slot) -> do
    ((result, commandState'), tcState') <-
      runTCM initEnv tcState $ runStateT action commandState
    pure (result, InteractionState tcState' commandState' slot)
```

(`interaction/AgdaMCP/Interaction/Internal.hs:92-100`). `CommandM` is
`StateT CommandState TCM`, so unwrapping it is one `runStateT` and one `runTCM`.
The fresh `IORef` that `runTCM` allocates is what makes the round trip sound:
the mutations a command performs land in that ref, and we take the final value
back out.

Agda does the same thing, once per command, inside `maybeAbort`
(`Interaction/InteractionTop.hs:304`) — it snapshots the `(TCState,
CommandState)` pair, runs the command against the snapshot, and writes the
result back on success. Agda already treats the session as a value. It just
never says so, and surrounds the fact with enough machinery to hide it.

### 4.3 What we do not need

**No thread and no queue.** Agda's reader thread and `TChan` exist for one
reason: `Cmd_abort` has to be readable while the main loop is busy running a
command, so it cannot be behind the same `getLine`. We do not support abort
(§8), so the whole apparatus goes. `CommandState` still has a `commandQueue`
field, so `newInteractionState` builds an inert one to fill it
(`interaction/AgdaMCP/Interaction/Internal.hs:60-62`); nothing ever reads it.

**No locks.** `InteractionM` is a bare `StateT ... IO` with no concurrency
control at all. This is sound only because of a property of the layer above:
**the MCP transports never run tool handlers concurrently.** That is an
assumption imported from the tool layer, and it is the reason a single mutable
session behind a state monad is safe. If it ever stops being true, this is the
first thing that breaks.

**No `optAbsoluteIncludePaths` staleness.** `repl` clears that field on the
options it stores for reload (`Interaction/AgdaTop.hs:43-46`) so that library
resolution runs afresh rather than reusing stale absolute paths.
`newInteractionState` does the same when seeding `optionsOnReload`
(`interaction/AgdaMCP/Interaction/Internal.hs:72-75`).

---

## 5. What runs around `interpret`

Sitting below `interpret` means the work `repl` does around it is now ours to
account for. The stack in the main loop is
`maybeAbort runInteraction`, where `runInteraction` dispatches through
`interpret` and wraps everything in `handleCommand`. This section walks all
three and gives each thing they do a verdict:

- **replicate** — we do the equivalent ourselves, and where.
- **free** — we get the same effect without doing anything, and why.
- **drop** — deliberately not done.
- **N/A** — cannot arise in our setting.

The tables are meant to be exhaustive. If something the interaction layer does
is missing from them, that is a bug in this document.

### 5.1 `maybeAbort` (`Interaction/InteractionTop.hs:304`)

| What it does | Verdict |
|---|---|
| Reads the next command off the queue | **N/A.** No queue. Wrapper calls arrive as function calls. |
| Snapshots `(TCState, CommandState)`, runs the command against the snapshot in `runTCM`, writes the states back on success | **Replicated, and generalized.** This is exactly `runCommandM` (§4.2). Where Agda does it once per queued command, we do it once per wrapper call, and it is the only way into Agda. |
| Races the command against an abort signal | **Dropped** with abort (§8). |
| On abort: drains lower-numbered commands, resets to persistent state, emits `Resp_DoneAborting` | **Dropped** with abort. Unreachable without the race. |

### 5.2 `runInteraction` (`Interaction/InteractionTop.hs:252-286`)

Five separable things.

| # | What it does | Verdict |
|---|---|---|
| 1 | **Implicit load** (`:257-263`): a non-`independent` command (`:412-419`) naming a file other than `theCurrentFile` type-checks that file first | **Dropped** (§8). Wrappers act on whatever session exists; ensuring the right file is loaded is the caller's job. |
| 2 | **Module-name reapplication** (`:262`, `:276`): `iotcm` is applied to `Nothing` for dispatch and re-applied to the real `TopLevelModuleName` so that ranges inside the command are correct | **N/A**, permanently — see below. |
| 3 | **`withCurrentFile`** (`:265`, `:925-929`): sets `envCurrentPath` from `theCurrentFile` around `interpret` | **Open issue** — see below. |
| 4 | **`Resp_InteractionPoints` emission** (`:267-271`) after commands in the `updateInteractionPointsAfter` table (`:424-466`), guarded on the file still being current | **Free.** The responses are for renumbering editor overlays. When we want the interaction points we query them. |
| 5 | **`onFail`** (`:284-286`): if an `independent` command fails, set `theCurrentFile = Nothing` (issue #853) | **Replicated** in the load wrapper's handler (`interaction/AgdaMCP/Interaction/Load.hs:100-108`). |

**On #2.** The obvious reading is that reapplication matters as soon as a
command carries a real range, which ours now do — `MakeCase.makeCase` is passed
`getInteractionRange goalId`
(`interaction/AgdaMCP/Interaction/MakeCase.hs:219`). That reading is wrong.
`parseIOTCM` (§1.2) shows what the mechanism is for: ranges *deserialized from
client text* carry a path but no module name, and the reapplication patches it
in. Our ranges are never deserialized. Every `Range` in the sublibrary either
comes out of Agda's own state (`getInteractionRange`, `getRange`, `fuseRange`
over Agda's abstract syntax) or is `noRange`, and Agda constructed all of those
with the module name already attached.

So the invariant that keeps this N/A is not "we do not pass real ranges." It is:
**no `Range` is ever constructed from caller-supplied data.** If a wrapper ever
needs to build a range from, say, a line/column pair sent by an agent, this
entry becomes live and the module name has to be attached by hand.

**On #3.** `Main.hs` previously claimed this was replicated. It is not: nothing
in `interaction/` or `src/` sets `envCurrentPath`, and `runCommandM` runs
everything under `initEnv`, where it is `Nothing`.

The exposure is narrow. `envCurrentPath` is forced by
`fromMaybe __IMPOSSIBLE__` at three places. One,
`getCurrentPath` (`TypeChecking/Monad/Env.hs:30-33`), is imported by
`Syntax/Translation/ConcreteToAbstract.hs:75` and never called — dead in this
checkout. The two live sites are
`Syntax/Translation/ConcreteToAbstract.hs:1406` and `:1729`, and both sit behind
short-circuiting conjunctions (`and2M`, `andM`) whose first conjunct is the
`--safe` flag. Reaching `__IMPOSSIBLE__` therefore requires **scope-checking a
declaration under `--safe`**. Our non-load wrappers scope-check expressions
(`parseExprIn`), not declarations, and load does not need `withCurrentFile`
because `typeCheckMain` sets `envCurrentPath` itself
(`Interaction/Imports.hs:1113`).

That argument is probably sound, but it is exactly the "gamble that those paths
are unreachable" that the design set out not to take, and `--safe` is reachable
— `Load.Request` forwards `requestArguments` to `cmd_load'`. **This is an open
issue, not a settled verdict**; the intended fix is to replicate
`withCurrentFile` in `runCommandM`, which removes the argument rather than
strengthening it.

### 5.3 `handleCommand` (`Interaction/InteractionTop.hs:157-243`)

Three things, all on the error path.

| What it does | Verdict |
|---|---|
| **Error response sequence** (`handleErr`, `:213-243`): emits `Resp_DisplayInfo (Info_Error ...)`, `Resp_JumpToError`, error and unsolved-meta highlighting, and a `Resp_Status` | **Dropped.** We catch the same `TCErr` and consume it directly, so nothing is lost. The highlighting is the only content we do not reproduce, and we do not want it (§8). |
| **`TCState` rollback preserving persistent state** (`:159`, `:176-179`): restore the pre-command `TCState` but keep the failed run's `stPersistentState` | **Free.** Built into `TCM`'s `catchError`; see §5.4. |
| **`handleNastyErrors`** (`:192-211`): converts non-`TCErr` exceptions into displayed errors (issue #637) and rethrows `AsyncCancelled` | **Dropped.** An unexpected exception is a bug in agda-mcp and should kill the process (§6.4). |

The last one deserves a note, because dropping it sounds more dangerous than it
is. Environmental failures still arrive as `TCErr`s: `TCMT`'s `liftIO` wraps
escaped `IOException`s into `TCErr.IOException`
(`TypeChecking/Monad/Base.hs:6099-6106`, constructor at `:5571`). The source
file disappearing mid-session is a catchable Agda outcome, not a crash. What
`handleNastyErrors` additionally catches is `SomeException` — genuine bugs —
and turning those into displayed errors is precisely the recovery we do not
want.

### 5.4 Why a plain `catchError` is transactional

The rollback in `handleCommand` is the one thing on the error path we genuinely
need, and we get it for nothing. Understanding why requires noticing that
`CommandM = StateT CommandState TCM` keeps its two pieces of state in
representations with completely different rollback stories.

**`CommandState` rolls back mechanically.** It is a real `StateT` layer, so
state is threaded through return values (`s -> m (a, s)`). A thrown `TCErr`
destroys the `(a, s')` pair that was the only thing carrying the updated state.
mtl's `catchError` for `StateT` is `liftCatch catchError`, and `liftCatch`
necessarily resumes the handler from the state at the catch site — this is the
uniformity property documented at `liftCatch` in `Control.Monad.Signatures`: a
lifting defined uniformly over an arbitrary base catch is handed only the
exception, so the entry state is the only state it *can* pass to the handler.
Rollback of `theCurrentFile`, `theInteractionPoints` and the rest is a
consequence of the representation, not a decision Agda made.

**`TCState` rolls back by policy.** It is not a transformer layer at all: it is
an `IORef` in `TCMT`'s reader (`TypeChecking/Monad/Base.hs:6031`,
`:6112-6115`), so its mutations survive exceptions. Rollback is written by hand
in `TCM`'s `MonadError` instance (`:6132-6146`) — save the state, restore it in
the handler, keep the failed run's `stPersistentState`, and skip the reset
entirely for `PatternErr`.

So wrapping a command in a plain `catchError` is transactional over both states.
`handleCommand` in fact *opts out* of the built-in rollback, catching with
`catchError_` — which "preserves the state of the failing computation"
(`TypeChecking/Monad/Base.hs:6186-6190`) — purely so that `handleErr` can
inspect the failed state to compute unsolved-meta highlighting, and then redoes
by hand (`:176-179`) the reset that `catchError` would have done for free. We
render errors from the `TCErr` itself, which carries its own captured state and
restores it during printing (`prettyError`, `TypeChecking/Errors.hs:99-100`), so
we never need the failed state and a plain `catchError` reaches the identical
end state with none of the machinery.

### 5.5 Two deliberate leaks, and `PatternErr`

The transaction is not perfect, and both imperfections are intentional in Agda:

1. **`stPersistentState` survives from the failed run.** This is the point of
   issue #2174: interfaces successfully loaded during a command that later
   failed should not have to be re-checked.
2. **`PatternErr` rolls back `CommandState` but not `TCState`**, because the
   `MonadError` instance skips the reset for it (`:6140-6141`). `PatternErr` is
   Agda's internal backtracking mechanism for constraint solving, and the
   comment at `catchPatternErr` (`:6126-6129`) records that preserving state
   there is a 20% performance difference on the standard library.

One escaping an interactive command would be an Agda-internal invariant
violation, so `catchTCErr` rethrows it rather than treating it as an outcome:

```haskell
catchTCErr :: CommandM a -> (TCErr -> CommandM a) -> CommandM a
catchTCErr action handler = action `catchError` handler'
 where
  handler' e@PatternErr {} = throwError e
  handler' e = handler e
```

(`interaction/AgdaMCP/Interaction/Internal.hs:105-109`). Every wrapper catches
through this, never through `catchError` directly.

One consequence is worth keeping in mind when writing a handler: **it runs in
the rolled-back `TCState`.** That is safe for error rendering only because the
`TCErr` carries its own state. Querying anything about the failed command's
effects from inside a handler would see pre-command data.

---

## 6. The shape of a wrapper

§3 ended with two requirements pulling against each other: a wrapper must run
inside `CommandM` to call Agda's own implementations, and must return data with
no dependency on `TCM`. Everything below follows from discharging both.

### 6.1 One module per command, with its own types

Each wrapper module exports a `Request`, a `Response`, and a single function
between them:

```haskell
load :: Request -> InteractionM Response
goal :: Request -> InteractionM Response
```

Not one sum type over all commands. Agda has that — `Interaction'` and
`Response_boot` — and it is part of why its responses cannot carry their own
meaning: a type that must describe fifty commands ends up describing none of
them precisely. `Load.Response` has three constructors because a load has
exactly three outcomes; `Goal.Response` is `Either GoalError GoalReport`
because a goal query has two.

`Request` and `Response` types derive `Eq` and `Show`. That is not incidental —
it is what lets the wrapper suite pin Agda's behaviour by equality, and the
wrapper suite is the project's oracle for what Agda does.

### 6.2 `Text` at the boundary

This is where the tension is discharged. A wrapper runs in `CommandM` and
therefore *can* hold `Doc`, `A.Expr`, `TCErr` and other things that only mean
something inside `TCM`. It must not hand any of them out. So every
`TCM`-dependent leaf is rendered to `Text` before the wrapper returns, in the
wrapper, using the same rendering call the frontend uses:

```haskell
extractGoalShape :: Rewrite -> InteractionId -> TCM GoalShape
extractGoalShape normalization goalId = do
  form <- typeOfMeta normalization goalId
  case form of
    OfType _ ty -> GoalOfType . Text.pack . render <$> prettyATop ty
    JustSort _ -> pure GoalSort
```

(`interaction/AgdaMCP/Interaction/Goal.hs:78-84`, mirroring `prettyTypeOfMeta`
at `Interaction/EmacsTop.hs:378-382`).

The result is that `AgdaMCP.Interaction.Model` is a module of plain data —
`Text`, `Bool`, `Int`, `InteractionId`, and our own `Span` — with no Agda
computation reachable from any of it. That is the property the tool layer
actually depends on.

It also means the *choice* of renderer is made once, inside the wrapper, next to
a citation of the frontend it copies. Compare §2.2, where the two frontends make
that choice separately and disagree.

### 6.3 The response reports everything its callers need

Because tools cannot inspect `InteractionState`, a wrapper's response has to be
sufficient. When a tool turns out to need something else about the session, the
correct fix is to **grow the response**, never to peek at the state or
re-derive the fact with tool-side IO.

Precedents: `LoadedFile` (with the source hash) on `Load.ResponseOk`; the hole
`Span` on every give-family response; `makeCaseReportSpan` and
`makeCaseReportCollapsesWhere` on the make-case report.

The re-derivation half of the rule matters as much as the peeking half. A tool
that re-read and hashed the file itself would open a window in which its hash
describes content Agda never checked.

### 6.4 The failure taxonomy

Three classes of failure, with different handling. Two of them are decided here.

1. **Agda outcomes are data.** A type error, an unsolvable goal, a warning, a
   stale load — these are things Agda is *supposed* to tell you, and they come
   back as ordinary constructors: `Load.ResponseError`, `Load.ResponseStale`,
   `GiveError`, `GoalError`. This is why every wrapper ends in a terminal
   `catchTCErr` and why response types are sums rather than exceptions.
2. **Caller misuse** — bad arguments, a stale `load_id` — is the tool layer's
   concern and is reported to the agent as an MCP error. It does not appear in
   this layer.
3. **Violations of our model of Agda are bugs.** They throw, nothing catches
   them, and the process dies. Examples:
   `SourceHashUnavailable` (`Load.hs:135`) when a module Agda just reported as
   loaded has no visited interface; `UnexpectedGoalConstraint` (`Goal.hs:94`)
   when `typeOfMeta` returns a constraint form that is documented not to occur;
   `MakeCaseNoClause` and `MakeCaseClauseNoRange` (`MakeCase.hs:270-275`) after
   `makeCase` has already succeeded.

Class 3 is the one that takes discipline. The temptation is always to return
something plausible instead. But a wrapper that quietly papers over a broken
assumption produces wrong Agda answers, and wrong Agda answers are the worst
possible failure mode for this program — an agent will believe them.

The scope of the `catchTCErr` matters for the same reason. In `load`, the catch
covers `cmd_load'` and the staleness read and nothing else
(`interaction/AgdaMCP/Interaction/Load.hs:82-93`), because those are the only
steps where a `TCErr` is a genuine load outcome. A `TCErr` thrown during
*extraction* is class 3, and widening the catch would silently reclassify it.

### 6.5 The one deliberate exception

`give_gen` reports its result — whether the hole should keep the user's text or
be replaced with an elaborated term — only by emitting `Resp_GiveAction`. It is
written nowhere in `TCState`, so there is no state to query afterwards.

The two requirements from §3 are in direct conflict here. Not reimplementing
`give_gen'` means the information can only arrive as a response; not consuming
responses means it cannot. Something has to give, and the no-reimplementation
requirement is the more important one, because forking `give_gen` is exactly the
drift that killed attempt 4.

So we install an output callback that captures precisely that one constructor
into an `IORef` (the `GiveSlot` from §4.1):

```haskell
captureGiveAction :: GiveSlot -> Response -> TCM ()
captureGiveAction slot (Resp_GiveAction pointId giveResult) =
  liftIO $ writeIORef slot $ Just (pointId, giveResult)
captureGiveAction _ _ = pure ()
```

(`interaction/AgdaMCP/Interaction/Internal.hs:87-90`). Every other response is
discarded.

This is the design working, not a hole in it. The rule was never "never touch
the response channel"; it was "tool code never sees Agda state or Agda
responses." The callback is installed once, in `Internal`, reads one
constructor, and writes into state the give-family wrappers own. Nothing above
the sublibrary can tell.

---

## 7. Staying faithful to the frontends

### 7.1 The policy

Every wrapper reimplements the extraction half of something a frontend does. The
default is to **mirror the Emacs frontend exactly**, and to cite the function
being mirrored in a comment next to the code doing it. We depart only when
either:

- the departure is an agda-mcp-specific improvement — usually because the Emacs
  behaviour depends on having a cursor and a buffer, which an agent does not
  have; or
- matching would cost more than it is worth, in which case the divergence is
  recorded here.

Emacs rather than JSON is the reference because it is the frontend with decades
of use behind its formatting decisions. Where the two disagree, we follow Emacs.

### 7.2 How fidelity is verified

**Citation discipline.** Extraction code names the frontend helper it mirrors,
by file and line. This is what makes the coupling checkable: a reader can open
`Interaction/EmacsTop.hs:378` and confirm that `extractGoalShape` does the same
thing. It is also what makes an Agda upgrade tractable, since the citations
enumerate what to re-check.

**Corpus discipline.** Any Agda-produced text pinned in a test expectation is
*observed* by driving the wrapper, never invented. Invented literals have twice
been wrong about Agda, and once drove a formatting decision against text that
does not exist. Shared payloads live in `Test.Corpus`, pinned against Agda in
`Test.Interaction`.

Between them: the citations say what we intended to copy, and the suite says
what Agda actually did.

### 7.3 Known divergences

**Make-case, inbound: we always pass the interaction range.** `agda2-make-case`
(`src/data/emacs-mode/agda2-mode.el:910-914`) goes through `agda2-goal-cmd`,
which sends `(agda2-mkRange nil)` — an empty range — whenever the input is not
taken from the goal text (`:772-773`, `:789`). For an empty hole, which is the
common case, the variables are read from the minibuffer and Emacs sends no
range. We always pass `getInteractionRange goalId`
(`interaction/AgdaMCP/Interaction/MakeCase.hs:219`). Harmless: the range's only
consumer is `parseVariables`, which uses it to update the metavariable's range.

**Make-case, outbound: we compute the clause span.** `Resp_MakeCase` carries no
span, so Emacs recovers the replacement target from the cursor —
`agda2-make-case-action` deletes from the line's indentation to
`line-end-position` (`agda2-mode.el:916-929`), under a docstring that openly
states the limitation: "Assumes that `<clause>` = `{!<variables>!}` is on one
line" (`:912`). We have no cursor and multi-line clauses are ordinary, so we
compute the real span from `ipClause` by fusing the clause's LHS and RHS ranges,
and report whether the replacement collapses a `where` block
(`extractClauseSpan`, `interaction/AgdaMCP/Interaction/MakeCase.hs:253-268`).
This is the paradigm case of the first clause of §7.1: the Emacs behaviour is an
approximation forced by editing at a cursor, and we can do better because we are
editing spans.

**Constraints follow Emacs over JSON.** `getConstraintsMentioning` results are
rendered with `prettyTCM` as Emacs does (`Interaction/EmacsTop.hs:245`), not
`pretty` as JSON does (`interaction/AgdaMCP/Interaction/Goal.hs:69-74`). A
choice between frontends rather than a departure from both, recorded because the
two can differ.

The tool layer has its own departures — enriching load results with per-goal
spans and contexts, sequent-style goal layout, showing inference and checking
together in `check`. Those are rendering and orchestration decisions above this
boundary and belong in the tool-layer document.

---

## 8. What we deliberately do not support

**Abort.** No `Cmd_abort`, no cancellation of a running command. A tool call
that starts type-checking runs to completion. This is what removes the reader
thread, the command queue, the `race`, and the abort branch of `maybeAbort`
(§4.3, §5.1). It is a real limitation — a long type-check cannot be interrupted
— and the reason it is acceptable is that MCP tool calls have their own timeout
at the client.

**Highlighting.** No `Resp_HighlightingInfo`, no aspects, no
`Indirect`/temp-file mechanism, no go-to-definition annotations. Syntax
highlighting is for a text editor. We get this for free rather than by
suppressing anything: `runCommandM` enters through `initEnv`, where
`envHighlightingLevel = None` (`TypeChecking/Monad/Base.hs:4145`), and we never
install the `inEmacs` wrapper that `runInteraction` uses to raise it.

**Implicit load.** `runInteraction` type-checks a file automatically when a goal
command names one that is not current (§5.2, item 1). Wrappers do not: they act
on whatever session exists. The reason is that an implicit load is a large,
slow, state-replacing operation, and having it happen as a side effect of a
goal query is exactly the kind of invisible state change this design exists to
prevent. Making the caller load explicitly is also what gives the tool layer a
place to enforce its staleness policy.

**Streaming progress.** No `Resp_RunningInfo`, no incremental status. A tool
call is unary: it returns once, with everything. This is a consequence of MCP's
shape, not a choice.

**The response channel generally.** Every response other than
`Resp_GiveAction` (§6.5) is discarded. Anything a wrapper needs is queried from
Agda's state, in the same way the frontend's renderer would have queried it.

---

## 9. Summary

- Agda's interaction layer streams imperatives to an editor and has no
  request/response function at any level; its `Response` type looks like a data
  boundary but is a rendering continuation that re-queries the type-checking
  state as it runs (§2).
- Consuming that stream, from another process or another thread, means either
  reimplementing Agda's model or shipping rendering code to where the state
  lives. Both were tried (§3).
- `TCState` and `CommandState` can be held as ordinary values between commands,
  which removes the thread, the queue, and the locks (§4).
- Coupling to Agda's internals is unavoidable. The design goal is to make the
  obligation closed rather than open-ended: Agda's invariants are maintained in
  one sublibrary of per-command wrappers, each citing the Agda code it mirrors,
  and nowhere else (§3, §6).
- A wrapper runs in `CommandM` so it can call Agda's own implementations, and
  returns plain data so its callers cannot touch Agda. Rendering to `Text` at
  the boundary is what reconciles those (§6.2).
- Everything the interaction layer does around `interpret` has been accounted
  for, with one open issue: `withCurrentFile` is not replicated (§5.2, item 3).
