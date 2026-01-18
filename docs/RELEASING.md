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

## Static Files in Release

The following static files must be included in release commits (as specified in `nix/frontend.nix`):

- `static/app.wasm` - Compiled WASM frontend
- `static/ghc_wasm_jsffi.js` - GHC WASM JavaScript FFI
- `static/index.js` - Application entry point
- `static/output.css` - Compiled Tailwind CSS
- `static/basecoat.cdn.min.css` - Basecoat UI CSS
- `static/mathjax-tex-svg.js` - MathJax for LaTeX rendering
- `static/wasi/` - WASI shim files

## Release Steps

1. **Determine version number**: Check `git log` since last release to determine if this is a major, minor, or patch release.

2. **Update versions** in all files listed above.

3. **Build the frontend** (in WASM shell):
   ```bash
   nix develop .#wasmShell.x86_64-linux
   ./deploy_frontend.sh
   ```

4. **Build and test the backend**:
   ```bash
   cabal build all
   cabal test all
   ```

5. **Create release commit**:
   ```bash
   git add \
     common/competences-common.cabal \
     backend/competences-backend.cabal \
     frontend/competences-frontend.cabal \
     nix/frontend.nix \
     static/app.wasm \
     static/ghc_wasm_jsffi.js \
     static/index.js \
     static/output.css \
     static/basecoat.cdn.min.css \
     static/mathjax-tex-svg.js \
     static/wasi/

   git commit -m "New release: X.Y.Z

   Changelog:
   - Feature 1
   - Feature 2
   - Bug fix 1
   "
   ```

6. **Push to remote**:
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
