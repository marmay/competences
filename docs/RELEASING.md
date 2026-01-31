# Release Process

This document describes how to create a new release of the competences application.

## Version Locations

Version numbers need to be updated in the following files:

1. **Cabal files** (use format `X.Y.Z.0`):
   - `common/competences-common.cabal`
   - `backend/competences-backend.cabal`
   - `frontend/competences-frontend.cabal`

2. **Nix file** (use format `X.Y.Z`):
   - `nix/frontend.nix`

## Repository Structure

Static assets (WASM binary, generated CSS, vendored libraries) live in a separate
repository [`competences-blobs`](https://github.com/marmay/competences-blobs),
included as a git submodule at `static/`.

Source files that feed into the build live in `frontend/static-src/`:
- `index.js` - WASM module loader (copied to `static/` by `deploy_frontend.sh`)
- `input.css` - Tailwind CSS input (compiled to `static/output.css`)

## Release Steps

1. **Determine version number**: Check `git log` since last release to determine if this is a major, minor, or patch release.

2. **Update versions** in all files listed above.

3. **Build the frontend** (in WASM shell):
   ```bash
   nix develop .#wasmShell.x86_64-linux
   ./deploy_frontend.sh
   ```

4. **Commit and push the blobs submodule**:
   ```bash
   cd static
   git add -A
   git commit -m "Release X.Y.Z"
   git push
   cd ..
   ```

5. **Update the Nix flake lock** (pins the new blobs commit):
   ```bash
   nix flake lock --update-input competences-blobs
   ```

6. **Build and test the backend**:
   ```bash
   cabal build all
   cabal test all
   ```

7. **Create release commit**:
   ```bash
   git add \
     common/competences-common.cabal \
     backend/competences-backend.cabal \
     frontend/competences-frontend.cabal \
     nix/frontend.nix \
     flake.lock \
     static

   git commit -m "New release: X.Y.Z

   Changelog:
   - Feature 1
   - Feature 2
   - Bug fix 1
   "
   ```

8. **Push to remote**:
   ```bash
   git push
   ```

## Generating Changelog

Review commits since the last release:
```bash
git log --oneline <last-release-commit>..HEAD
```

Summarize significant changes in the commit message, grouped by:
- New features
- Improvements
- Bug fixes
- Breaking changes (if any)

## Reproducing Historical Releases

Every commit from the first release onwards has a `static` submodule entry
pointing to the correct blobs commit. To reproduce any historical release:

```bash
git checkout <release-commit>
git submodule update --init
```
