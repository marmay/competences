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

The frontend's `static/` tree (WASM binary, generated CSS, MathJax bundle,
vendored fonts/wasi shim) is produced reproducibly from source:

```bash
nix build .#competences-frontend
```

Vendored static assets (third-party fonts, the WASI shim) live in
`frontend/static-src/{fonts,wasi}/`. Source files that feed into the build:

- `frontend/static-src/index.js` — WASM module loader, bundled by esbuild.
- `frontend/static-src/input.css` — Tailwind input, compiled to `output.css`.

`nix/npm-deps.nix` pulls MathJax and basecoat-css via `buildNpmPackage` from
the pinned `package-lock.json`. The dev shell symlinks the same Nix-built
`node_modules` into the project root so `deploy_frontend.sh` and the
production derivation share one source.

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

Run native build and tests as a fast feedback step:
```bash
nix develop --command cabal build all
nix develop --command cabal test all
```

### 4. Verify the frontend derivation builds

```bash
nix build .#competences-frontend
```

This produces `result/` with the same shape as the runtime `static/` tree
(`app.wasm`, `ghc_wasm_jsffi.js`, `index.js`, `output.css`, `mathjax-*`,
`fonts/`, `wasi/`). The build is hermetic — caches into the IOG / nixpkgs
caches, no manual artifact wrangling.

### 5. Create release commit

Stage the 6 version files plus `flake.lock` (in case dep updates were
folded in) and commit:

```bash
git add \
  common/competences-common.cabal \
  markdown/competences-markdown.cabal \
  backend/competences-backend.cabal \
  frontend/competences-frontend.cabal \
  housecup/competences-housecup.cabal \
  nix/frontend.nix \
  flake.lock
```

Generate a changelog by reading `git log --oneline <prev-release>..HEAD` and
categorizing commits into the sections below. Omit any category with no entries.
Commit with the standard format (see Changelog Format below).

### 6. Push to remote

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

Every release commit pins haskell.nix, nixpkgs, and `package-lock.json` via
`flake.lock`. To reproduce any historical release:

```bash
git checkout <release-commit>
nix build .#competences-frontend  # rebuilds the static/ tree from scratch
nix build .#competences-backend
```

The first run on a cold cache may need to build the WASM cross GHC from
source (~30 min); subsequent runs are cache-fast.
