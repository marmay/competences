# Debugging Guide

This file contains detailed debugging information and lessons learned during development.

> **Note for Claude Code**: When encountering compilation errors or runtime issues, search this file for relevant error messages and solutions.

## General Development Workflow

1. **Always read files before editing** - Use the Read tool to understand existing code structure
2. **Build incrementally** - Fix one error at a time, rebuild frequently
3. **Check dependencies** - Missing packages in `.cabal` files are a common issue
4. **Enable all extensions** - Ensure executable sections have same extensions as library (especially `OverloadedLabels`)

## NoFieldSelectors + OverloadedRecordDot Pattern

When using both extensions together:

```haskell
-- Import constructor to enable field access
import MyModule (MyType(..))  -- Note the (..)

-- Now you can use record dot syntax
value = myRecord.field.nestedField

-- For newtypes like Id:
import Competences.Document.Id (Id(..))  -- Enables .unId
uuid = userId.unId
```

**Why this matters:**
- `NoFieldSelectors` disables automatic field selector generation
- `OverloadedRecordDot` provides `.` syntax as an alternative
- But you MUST import the constructor for GHC to know about the fields

**Common Errors:**
```
ERROR: "No instance for 'HasField "fieldName" r0 Type'"
FIX: Import the data type constructor with (..) - e.g., Type(..)

ERROR: "Notice that 'fieldName' is a field selector... that has been suppressed by NoFieldSelectors"
FIX: Import the constructor: import Module (Type(..))
```

## WASM Frontend Development

**Build Command:**
```bash
./deploy_frontend.sh
```

**Common Issues:**

1. **Missing dependencies in WASM build:**
   - Check executable's `if flag(wasm)` section in `.cabal` file
   - Need: `jsaddle`, `jsaddle-wasm`, `optics-core`

2. **Type defaulting errors:**
   - Always add explicit type annotations to string literals: `("text" :: Text)`
   - Affects all JSaddle FFI calls (property names, method names)

3. **Import errors with OverloadedRecordDot:**
   - Import data constructors: `Type(..)` not just `Type`
   - Example: `import Competences.Document.Id (Id(..))` enables `.unId` access

**Common Compilation Errors:**
```
ERROR: "Couldn't match expected type: JSM JSVal with actual type: args0 -> JSM JSVal"
FIX: Use correct operator precedence and argument passing for # operator

ERROR: "Defaulting the type variable 'name0' to type '[Char]'"
FIX: Add explicit type annotations: ("propertyName" :: Text)

ERROR: "Could not load module 'jsaddle'" (in WASM build)
FIX: Add jsaddle and optics-core to executable build-depends under wasm flag

ERROR: "Variable not in scope: (#)"
FIX: Add OverloadedLabels to executable's default-extensions

ERROR: "Couldn't match type 'IO' with 'JSM'"
FIX: Use liftIO for IO operations: liftIO $ putStrLn "message"
```

## JSaddle FFI Quick Reference

```haskell
-- Imports needed
import Language.Javascript.JSaddle
  ( JSM, JSVal, fun, jsg, new, toJSVal, valToText, (!), (#), (<#) )

-- Global object access
window <- jsg ("window" :: Text)

-- Property access (reading)
value <- obj ! ("propertyName" :: Text)

-- Property setting
_ <- obj <# ("propertyName" :: Text) $ someValue

-- Constructor calls (like 'new WebSocket(url)')
ws <- new (jsg ("WebSocket" :: Text)) [toJSVal url]

-- Method calls (like 'ws.send(data)')
jsonVal <- toJSVal textData
_ <- ws # ("send" :: Text) $ [jsonVal]

-- Creating callbacks
callback <- fun $ \this fn args -> do
  case args of
    (firstArg:_) -> do
      -- handle event
      pure ()
    _ -> pure ()

-- Setting event handlers
_ <- element <# ("onclick" :: Text) $ callback
```

## JWT Token Issues

**Common Pitfalls:**
- Using `show` on newtypes creates Haskell-formatted strings like `"Id {unId = ...}"`
- Forgetting to import constructors with `NoFieldSelectors` prevents field access
- Not using `stringOrURIToText` for JWT claims extraction
- `OverloadedRecordDot` requires constructor import: `Id(..)` enables `value.unId` syntax

**Check JWT contents:**
```bash
# Decode JWT (just the payload, base64)
echo "eyJ..." | base64 -d
```

**Common JWT issues:**
- User ID not found: Check `sub` field is valid UUID (not `"Id {unId = ...}"`)
- Authentication fails: Verify JWT secret matches between generation and validation
- User not linked: Office365 ID must match a user in the database

**Correct Implementation:**

Import Requirements:
```haskell
import Competences.Document.Id (Id (..), mkId)  -- Must import constructor
import Data.UUID.Types qualified as UUID
import Web.JWT (stringOrURIToText)
```

Generating JWT:
```haskell
-- ❌ WRONG - creates "Id {unId = <uuid>}"
JWT.sub = JWT.stringOrURI $ T.pack $ show user.id

-- ✅ CORRECT - creates just the UUID string
JWT.sub = JWT.stringOrURI $ UUID.toText user.id.unId
```
- With `NoFieldSelectors` + `OverloadedRecordDot`, you MUST import `Id(..)` constructor to access `.unId`
- Use `UUID.toText` to convert UUID to Text (NOT `show`)

Extracting from JWT:
```haskell
-- ❌ WRONG - double-wraps the value
Just uri -> Right $ T.pack $ show uri

-- ✅ CORRECT - properly extracts the text
Just uri -> Right $ stringOrURIToText uri
```
- Use `stringOrURIToText` from `Web.JWT` to extract Text from StringOrURI
- Then use `mkId` to parse the UUID string

## WebSocket Connection Debugging

**Frontend (Browser Console):**
```javascript
// Check if JWT is set
console.log(window.COMPETENCES_JWT)

// Check WebSocket connection
// Should see: "WebSocket connected"
```

**Backend (Terminal):**
```
# Should see:
Client connected: <name> (<user-id>)

# If authentication fails:
Authentication failed: <error message>
```

**Common Connection Issues:**

1. **400 Bad Request:**
   - Check WebSocket URL includes `/` before query: `ws://host:port/?token=...`
   - Verify JWT token is being passed correctly

2. **401 Unauthorized:**
   - JWT validation failed
   - Check JWT secret matches
   - Check JWT hasn't expired

3. **Connection refused:**
   - Backend not running
   - Wrong port number
   - Firewall blocking WebSocket

## PostgreSQL / Database Issues

**Pattern matching errors with query results:**
```
ERROR: "No instance for 'FromField (Int64, ByteString)'"
FIX: query_ returns a list of tuples, pattern match with (x, y) : _ not [(x, y) : _]
```

**Missing imports:**
```
ERROR: "Variable not in scope: diffUTCTime"
FIX: Add to Data.Time import: import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
```

## Cabal / Build Configuration

**Missing extensions in executables:**
```
ERROR: "No instance for 'HasField ...'" in executable but library builds fine
FIX: Add default-extensions to executable stanza matching library:
  DuplicateRecordFields,
  GADTs,
  NoFieldSelectors,
  OverloadedRecordDot,
  OverloadedStrings,
  TypeFamilies
```

**Type annotation needed for JSON decoding:**
```
ERROR: "Ambiguous type variable 'r0' arising from a use of 'eitherDecodeFileStrict'"
FIX: Add type annotation: result <- eitherDecodeFileStrict path :: IO (Either String MyType)
```
