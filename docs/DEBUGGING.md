# Debugging and Common Pitfalls

This document lists common errors you might encounter when working with this codebase and how to fix them.

## Haskell Language Extensions

### NoFieldSelectors + OverloadedRecordDot

**Error:**
```
No instance for 'HasField "field" Type ...'
```

**Fix:**
Import the constructor with `(..)` to enable record field access:
```haskell
-- ✓ CORRECT
import Competences.Document (Document(..), User(..))

-- ✗ WRONG (won't allow doc.users access)
import Competences.Document (Document, User)
```

**Why:** With `NoFieldSelectors` enabled, field selectors are not automatically generated. The `OverloadedRecordDot` extension relies on `HasField` instances. Importing constructors with `(..)` enables the dot syntax.

### Missing Extensions in Executables

**Error:**
```
HasField errors in executable but library builds fine
```

**Fix:**
Add `default-extensions` to the executable stanza in the `.cabal` file, matching the library:
```cabal
executable competences-frontend
    default-extensions:
        DuplicateRecordFields,
        GADTs,
        NoFieldSelectors,
        OverloadedLabels,
        OverloadedRecordDot,
        OverloadedStrings
```

**Why:** Extensions enabled in the library stanza don't automatically apply to executable stanzas.

## JSaddle FFI and Type Defaulting

### Type Defaulting in JSaddle FFI

**Error:**
```
Defaulting the type variable 'name0' to type '[Char]'
  arising from a use of 'fromString'
```

**Fix:**
Add explicit type annotations to string literals in FFI calls:
```haskell
-- ✓ CORRECT
obj ! ("propertyName" :: Text)

-- ✗ WRONG
obj ! "propertyName"  -- Type ambiguous
```

**Why:** JSaddle's FFI functions are polymorphic over the string type, and GHC can't infer which type you want. Explicitly annotate with `Text` or `JSString` to avoid defaulting warnings.

### JSaddle Property Access

**Pattern:**
```haskell
import Data.Text (Text)
import Language.Javascript.JSaddle (JSM, (!), js, valToText)

-- Reading a property
readProperty :: JSVal -> JSM Text
readProperty obj = valToText =<< obj ! ("propertyName" :: Text)

-- Setting a property
setProperty :: JSVal -> Text -> JSM ()
setProperty obj value = obj <# ("propertyName" :: Text) $ value
```

## PostgreSQL and Database

### Pattern Matching on Query Results

**Error:**
```
No instance for 'FromField (Int64, ByteString)'
  arising from a use of 'query_'
```

**Fix:**
Pattern match correctly on query results - they're already tuples in a list:
```haskell
-- ✓ CORRECT
result <- query_ conn "SELECT id, data FROM table" :: IO [(Int64, ByteString)]
case result of
  (id, data) : _ -> ...  -- First element
  [] -> ...              -- Empty result

-- ✗ WRONG
case result of
  [(id, data) : _] -> ...  -- Extra list layer
```

**Why:** `query_` returns `IO [row]` where each `row` is already a tuple. Don't add an extra layer of list pattern matching.

## JWT Token Handling

### Never Use `show` on Newtypes

**Error:**
```
JWT contains "Id {unId = ...}" instead of UUID string
```

**Fix:**
Use proper conversion functions, not `show`:
```haskell
-- ✓ CORRECT - Generating JWT
import Competences.Document.Id (Id (..), mkId)
import Data.UUID.Types qualified as UUID

JWT.sub = JWT.stringOrURI $ UUID.toText user.id.unId  -- NOT show user.id

-- ✓ CORRECT - Extracting from JWT
import Web.JWT (stringOrURIToText)

Just uri -> Right $ stringOrURIToText uri  -- NOT show uri
```

**Why:** `show` on a newtype includes the constructor name, producing `"Id {unId = <uuid>}"` instead of just `"<uuid>"`. Always unwrap newtypes and use appropriate conversion functions.

### JWT Debugging

**Check JWT contents:**
```bash
# Decode JWT (header.payload.signature)
echo '<jwt-token>' | cut -d. -f2 | base64 -d | jq
```

**Common JWT issues:**
- Expired tokens: Check `exp` claim
- Invalid user ID: Check `sub` claim format
- Missing claims: Check `role`, `name`, `office365Id`
- Wrong secret: Backend can't verify signature

## Build and Compilation

### Cached Build Errors

**Problem:**
Cabal shows errors from old cached builds that don't reflect current code.

**Fix:**
Run a fresh blocking build:
```bash
# Stop background builds
pkill -f "cabal build"

# Run fresh build
cabal build competences-frontend
```

### Unused Import Warnings

**Error:**
```
The import of 'Module' is redundant
  except perhaps to import instances from 'Module'
```

**Fix:**
Remove the import or change to instance-only import:
```haskell
-- If truly unused
-- import Module  -- DELETE THIS LINE

-- If only needed for instances
import Module ()  -- Import instances only
```

**Why:** With `-Werror`, warnings become errors. Remove genuinely unused imports to keep code clean.

## Frontend WebSocket Integration

**Status:** ✅ Fully functional

### WebSocket Connection Issues

**Problem:** WebSocket fails to connect or disconnects immediately.

**Debug steps:**
1. Check JWT is being passed: `ws://host:port/?token=<jwt>`
2. Verify JWT is valid (not expired, correct secret)
3. Check browser console for WebSocket errors
4. Check backend logs for connection rejections

### FFI Callback Pattern

**Pattern for handling WebSocket messages:**
```haskell
import Language.Javascript.JSaddle (JSM, jsg, js, js1, fun, valToText)

setupWebSocket :: JSM ()
setupWebSocket = do
  ws <- jsg ("WebSocket" :: Text) # new (wsUrl :: Text)

  -- onmessage handler
  let handleMessage = fun $ \_ _ [evt] -> do
        msg <- evt ! ("data" :: Text) >>= valToText
        -- Handle message
        pure ()

  ws <# ("onmessage" :: Text) $ handleMessage
```

## Authentication and Authorization

### Office365 OAuth Issues

**Problem:** Login fails even with valid Office365 account.

**Check:**
1. User exists in database with matching `office365Id`
2. Email from Microsoft Graph matches user's `office365Id` field
3. OAuth app has correct permissions: `User.Read`, `openid`, `profile`, `email`
4. Redirect URI matches exactly (including http/https)

### Command Authorization

**Current:** All commands require Teacher role.

**Error:** `CommandRejected: "Only teachers can execute commands"`

**Fix:** Ensure user has Teacher role, or wait for student command authorization (future work).

## Common Optics Errors

### Lens Composition

```haskell
-- ✓ CORRECT
doc & #users % at userId ?~ newUser

-- ✗ WRONG
doc & #users . at userId .~ Just newUser  -- Mixing optics-core and lens operators
```

**Why:** This codebase uses `optics-core`, not `lens`. Use `%` for composition, not `.`.

### Setting vs Modifying

```haskell
-- Setting a field
user & #name .~ "New Name"

-- Modifying a field
user & #age %~ (+1)

-- Getting a field
user ^. #name
```

## WASM Build Issues

### Missing WASM Toolchain

**Error:** `wasm32-wasi-cabal: command not found`

**Fix:**
```bash
# Enter WASM development shell
nix develop .#wasmShell.x86_64-linux

# Now wasm32-wasi-cabal is available
wasm32-wasi-cabal build
```

### Resource Loading in WASM

**Problem:** WASM files fail to load when served from `/oauth/callback`.

**Fix:**
Use absolute paths in `static/index.js`:
```javascript
// ✓ CORRECT
const wasmUrl = '/static/app.wasm';
const jsUrl = '/static/ghc_wasm_jsffi.js';

// ✗ WRONG (relative paths fail from /oauth/callback)
const wasmUrl = 'app.wasm';
```

## Debugging Techniques

### Enable Debug Logging

**Frontend (browser console):**
```haskell
import Debug.Trace (traceShow, traceShowId)

-- Trace value
result = traceShow ("Debug:", value) $ computation value

-- Trace and return value
result = traceShowId $ computation value
```

**Backend (stdout):**
```haskell
import Debug.Trace (traceShowIO)

-- In IO context
traceShowIO ("Debug:", value)
```

### Inspect Document State

**In browser console (when using JSaddle dev mode):**
```javascript
// Get current document
window.hsDebug.getDocument()

// Get sync document state
window.hsDebug.getSyncDocument()
```

### PostgreSQL Query Debugging

**Enable query logging:**
```sql
-- In postgresql.conf
log_statement = 'all'
log_duration = on

-- Then restart PostgreSQL and check logs
```

**Check query plans:**
```sql
EXPLAIN ANALYZE SELECT * FROM commands WHERE generation > 1000;
```

### WebSocket Message Inspection

**Browser DevTools:**
1. Open DevTools → Network tab
2. Filter by "WS" (WebSocket)
3. Click on WebSocket connection
4. View Messages tab to see all sent/received messages

## Performance Issues

### Slow Startup

**Check:**
1. How many commands since last snapshot? Should be < 25
2. Is snapshot creation working? Check `snapshots` table
3. Database query performance: check `EXPLAIN ANALYZE` on command queries

### High Memory Usage

**Check:**
1. Document size: how many entities?
2. Frontend: are old subscriptions being cleaned up?
3. Backend: connection pool size (default 3 connections)

## Property-Based Testing

**Future work:** Add QuickCheck properties to verify:
- `AffectedUsers` correctness
- Document projection correctness
- Command replay correctness

**Example property:**
```haskell
prop_affectedUsersMatchProjection :: User -> Command -> Document -> Property
prop_affectedUsersMatchProjection sender cmd doc =
  case handleCommand sender.id cmd doc of
    Right (doc', AffectedUsers affected) ->
      let changedProjections = filter
            (\u -> projectDocument u doc /= projectDocument u doc')
            allUsers
      in sort affected === sort (map (.id) changedProjections)
```
