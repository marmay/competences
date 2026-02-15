# Release Process

This document describes how to create a new release of the competences application.
It is written as step-by-step instructions that Claude Code can execute directly
(requesting user permission for destructive or external commands).

## Version Locations

Version numbers need to be updated in **6 files**:

| File | Format | Example |
|------|--------|---------|
| `common/competences-common.cabal` | `X.Y.Z.0` | `version: 0.11.0.0` |
| `markdown/competences-markdown.cabal` | `X.Y.Z.0` | `version: 0.11.0.0` |
| `backend/competences-backend.cabal` | `X.Y.Z.0` | `version: 0.11.0.0` |
| `frontend/competences-frontend.cabal` | `X.Y.Z.0` | `version: 0.11.0.0` |
| `housecup/competences-housecup.cabal` | `X.Y.Z.0` | `version: 0.11.0.0` |
| `nix/frontend.nix` | `X.Y.Z` | `version = "0.11.0";` |

**Note:** `BuildInfo.hs` files auto-derive their version from the cabal `Paths_`
modules and require no manual update. The `csvconvert` package has its own
independent version (`0.1.0.0`) and is not part of the release cycle.

## Repository Structure

Static assets (WASM binary, generated CSS, vendored libraries) live in a separate
repository [`competences-blobs`](https://github.com/marmay/competences-blobs),
included as a git submodule at `static/`.

Source files that feed into the build live in `frontend/static-src/`:
- `index.js` - WASM module loader (copied to `static/` by `deploy_frontend.sh`)
- `input.css` - Tailwind CSS input (compiled to `static/output.css`)

## Release Steps

### 1. Determine version number

Read the git log since the last release and propose a version bump to the user:
```bash
git log --oneline <last-release-commit>..HEAD
```
Ask the user to confirm the new version number (major, minor, or patch bump).

### 2. Update versions

Use the Edit tool to update the version string in all 6 files listed above.
Two formats are used:
- `.cabal` files: `X.Y.Z.0` (four-part, e.g., `0.12.0.0`)
- `nix/frontend.nix`: `X.Y.Z` (three-part, e.g., `0.12.0`)

### 3. Build and test

Run a native build and tests as a fast feedback step before the slow WASM build:
```bash
cabal build all && cabal test all
```

### 4. Build frontend WASM

This compiles the WASM binary, runs wasm-opt/wasm-tools, copies `index.js`,
and builds Tailwind CSS. Takes several minutes (compiles ~170 Haskell modules).
Requires user permission (enters a Nix shell):
```bash
nix develop .#wasmShell.x86_64-linux -c ./deploy_frontend.sh
```

### 5. Commit and push the blobs submodule

Requires user permission for the `git push`:
```bash
cd static && git add -A && git commit -m "Release X.Y.Z" && git push && cd ..
```

### 6. Update the Nix flake lock

This pins the new blobs commit in `flake.lock`. Requires user permission:
```bash
nix flake update competences-blobs
```

### 7. Create release commit

Stage exactly these 8 paths (the 6 version files + flake.lock + submodule pointer):
```bash
git add \
  common/competences-common.cabal \
  markdown/competences-markdown.cabal \
  backend/competences-backend.cabal \
  frontend/competences-frontend.cabal \
  housecup/competences-housecup.cabal \
  nix/frontend.nix \
  flake.lock \
  static
```

Generate a changelog by reading `git log --oneline <prev-release>..HEAD` and
categorizing commits into the sections below. Omit any category with no entries.
Commit with the standard format (see Changelog Format below).

### 8. Push to remote

Requires user permission:
```bash
git push
```

## Changelog Format

The release commit message should follow this structure:

```
New release: X.Y.Z

Changelog:

New features:
- ...

Improvements:
- ...

Refactoring:
- ...

Bug fixes & quality:
- ...
```

Generate the changelog by reading `git log --oneline` since the last release
and grouping commits into these categories. Omit any category that has no entries.

## Reproducing Historical Releases

Every commit from the first release onwards has a `static` submodule entry
pointing to the correct blobs commit. To reproduce any historical release:

```bash
git checkout <release-commit>
git submodule update --init
```
