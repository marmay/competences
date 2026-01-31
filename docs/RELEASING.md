# Release Process

This document describes how to create a new release of the competences application.

## Version Locations

Version numbers need to be updated in **4 files**:

| File | Format | Example |
|------|--------|---------|
| `common/competences-common.cabal` | `X.Y.Z.0` | `version: 0.8.0.0` |
| `backend/competences-backend.cabal` | `X.Y.Z.0` | `version: 0.8.0.0` |
| `frontend/competences-frontend.cabal` | `X.Y.Z.0` | `version: 0.8.0.0` |
| `nix/frontend.nix` | `X.Y.Z` | `version = "0.8.0";` |

## Repository Structure

Static assets (WASM binary, generated CSS, vendored libraries) live in a separate
repository [`competences-blobs`](https://github.com/marmay/competences-blobs),
included as a git submodule at `static/`.

Source files that feed into the build live in `frontend/static-src/`:
- `index.js` - WASM module loader (copied to `static/` by `deploy_frontend.sh`)
- `input.css` - Tailwind CSS input (compiled to `static/output.css`)

## Release Steps

### 1. Determine version number

Review commits since the last release to decide major/minor/patch:
```bash
git log --oneline <last-release-tag-or-commit>..HEAD
```

### 2. Update versions

Update the version string in all 4 files listed above.

### 3. Build the frontend

This compiles the WASM binary, runs wasm-opt/wasm-tools, copies `index.js`,
and builds Tailwind CSS. Takes several minutes (compiles ~170 Haskell modules).

```bash
nix develop .#wasmShell.x86_64-linux -c ./deploy_frontend.sh
```

### 4. Commit and push the blobs submodule

```bash
cd static
git add -A
git commit -m "Release X.Y.Z"
git push
cd ..
```

### 5. Update the Nix flake lock

This pins the new blobs commit in `flake.lock`:
```bash
nix flake update competences-blobs
```

### 6. Build and test

Verify everything compiles and tests pass with the new version numbers:
```bash
cabal build all
cabal test all
```

### 7. Create release commit

Stage exactly these 6 paths (the 4 version files + flake.lock + submodule pointer):
```bash
git add \
  common/competences-common.cabal \
  backend/competences-backend.cabal \
  frontend/competences-frontend.cabal \
  nix/frontend.nix \
  flake.lock \
  static
```

Commit with a changelog (see format below):
```bash
git commit
```

### 8. Push to remote

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

Generate the changelog by reviewing `git log --oneline` since the last release
and grouping commits into these categories. Omit any category that has no entries.

## Reproducing Historical Releases

Every commit from the first release onwards has a `static` submodule entry
pointing to the correct blobs commit. To reproduce any historical release:

```bash
git checkout <release-commit>
git submodule update --init
```
