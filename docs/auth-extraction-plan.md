# Extracting the auth service into `marmay-auth`

## Context and goal

The shared auth service (Stage 1 of [teams-integration-plan.md](teams-integration-plan.md),
complete and verified 2026-07-24) moves into its own repository `marmay-auth` so the planned
CMS can reuse it. The extraction is **more than moving the service**: consumers (competences
instance today, CMS next) need the *verifier half* of the protocol — assertion validation,
replay protection, the client bootstrap, the config-file loader. So `marmay-auth` is a
**library + executable**, and competences-backend depends on the library.

What stays in competences: `Backend/Auth.hs` (session JWT — instance-local session
machinery, deliberately NOT part of the auth protocol), `Backend/SecurityConfig.hs` (imports
the loader from marmay-auth), the app shell (composes the bootstrap core with app-specific
hooks), and the instance-side nginx `/auth/` proxy config.

## Ordering (deviates from the first sketch — rename BEFORE move)

Renaming after the move would churn the competences imports twice (once at extraction, again
at rename) and would rename files in a fresh repo's first commits, wrecking `git log
--follow` continuity. Instead: do all refactoring **in place** in the competences repo, where
every step is a small, buildable, individually testable commit — then the extraction itself
is pure file relocation plus build plumbing.

### Phase A — in competences (each step builds + commits separately)

**A1. Fold `Competences.Internal.SecurityConfig` into the Auth namespace.**
`Competences.Auth.ConfigFile` (name it for what it does — the permission-checked JSON
loader; `forceLoadSecurityConfig` can become just `forceLoadConfigFile`). Both
`Auth/SecurityConfig.hs` and `Backend/SecurityConfig.hs` import it from there. Delete
`Competences/Internal/`.

**A2. Split the bootstrap into protocol core + app hooks.** — DONE 2026-07-24 (verified in
browser). `runAuthBootstrap`/`showPanel`/`INDEX_JS` are page globals now (no IIFE) so the
core can later become a separately served file; network-vs-server failure both dispatch to
`onFailure` (one message).
New `Competences.Auth.Bootstrap` renders the protocol core as a JS **function definition**,
e.g. `runAuthBootstrap(opts)` with `opts = { authBase, loginPath, storageKey, onToken(jwt),
onNoAccount(), onFailure(retryUrl) }`:
- Core owns: fragment extraction + scrub, `POST loginPath` exchange (octet-stream header),
  error-code dispatch (`unknown-user` → `onNoAccount`, else `onFailure`), sessionStorage
  caching, the client-side `exp` check (60 s margin), the redirect-vs-panel loop-protection
  rule, dev mode (`authBase` null → `onToken(null)`).
- `Backend/Shell.hs` keeps: the `opts` call site with app values (`storageKey:
  'competences.sessionJwt'`, `loginPath: '/api/login'`), module-script injection in
  `onToken`, panel rendering (German texts) in the two error hooks.
- sessionStorage is per-origin, so the key needs no cross-app uniqueness — it is a parameter
  simply so each app's choice is visible at its call site.
- Re-test in the browser after this step; it is the riskiest refactor of Phase A.

**A3. Move replay protection into the Auth namespace.** — DONE 2026-07-24 as
`Competences.Auth.ReplayProtection` (`ConsumedLog`, `mkConsumedLog`, `ensureUnconsumed`).
The unit test is DEFERRED to B1 (no test infrastructure in competences yet; the marmay-auth
repo gets a real test suite from day one — the ReplayProtection cases are its first tests,
non-negotiable there).
`Competences.Auth.Jti`: the consumed-`jti` set (`newConsumedJtiSet`, `consumeAssertionId` —
the current `ensureUnconsumed` logic including the prune-in-one-STM-transaction property).
`RestState` holds the set by this type; drop the logic from `Backend/State.hs`. **Write the
pending unit test now** (same-id-twice → False; expired entries pruned) — it travels into
the new repo where the protocol's tests belong. Rationale for moving: replay protection is
part of the protocol contract, and it already produced one invisible-in-manual-testing bug
(inverted prune predicate); the CMS must not reimplement it.

**A4. (Optional but recommended) `VerifierConfig`.**
A record bundling what every consumer needs: `authPublicKey`, `origin`,
`allowedExpirySkewDuration`, `authBaseUrl`. `Backend/SecurityConfig` embeds it as one field
(flatten in JSON or nest — nested keeps consumer configs uniform across apps). This is the
"how do I consume marmay-auth" API: public key + origin + skew + base URL, nothing else.

**A5. Rename `Competences.Auth.*` → `Marmay.Auth.*` in place.** — DONE 2026-07-24 (incl.
`iss` → `"marmay-auth"` at both sites). Mechanical
(find/replace + `git mv`), one commit. Do the **wire-protocol renames in the same commit**,
because they are two-sided and both sides still live in one repo here — after extraction
they would need coordinated releases:
- `iss` claim: `"competences-auth"` → `"marmay-auth"` (minted in `Assertion.hs`, checked in
  the same module — one constant; safe now because assertions live 60 s and there is a
  single deployment).
- Claim URI `https://auth.bu-ki.at/#userName`: fine as-is (domain-based, service-neutral) —
  keep.

### Phase B — the extraction

**B1. Create the `marmay-auth` repo.** Layout:

```
marmay-auth.cabal        library (Marmay.Auth.*) + executable marmay-auth
lib/Marmay/Auth/         Assertion, Microsoft, OAuth2Config, SecurityConfig,
                         ConfigFile, HTTP, Bootstrap, Jti
app/Main.hs              from backend/app-auth/
test/                    Jti test (A3), Assertion round-trip test (cheap to add:
                         generate → validate, wrong aud → rejected, expired → rejected)
flake.nix                package + nixosModule
```

Flake: **haskell.nix**, mirroring competences' setup (familiarity wins over minimalism —
same `cabal.project` + materialization workflow, same debugging knowledge applies; the cost
is the heavier eval/first-build, already accepted in the parent repo). Pin GHC to match
competences (9.14.1) so both build from one toolchain.

**nixosModule** (resolves the deferred packaging from the teams plan): `services.marmay-auth
= { enable, port, secretsFile }` — systemd unit (DynamicUser, config via `secretsFile`
path-permission conventions the loader already enforces), nginx vhost for the shared domain
routing `/auth/` to the service. The **instance-side** `/auth/` proxy location (for the
Stage 2 same-origin Teams exchange) stays in competences' `nix/module.nix` — it is
instance-vhost config.

**B2. Wire competences to the new repo.**
- Delete `backend/lib/Competences/Auth/` (now `Marmay/Auth/`) and `backend/app-auth/` from
  competences; drop the `competences-auth` executable stanza; remove now-unneeded deps from
  the library stanza (check: `network-uri`, `cookie`, `http-client-tls` — some remain used
  by the instance side).
- `cabal.project`: `source-repository-package` pointing at the marmay-auth git URL + pinned
  tag/rev (haskell.nix consumes this directly; add the `--sha256:` comment it needs for
  reproducible fetching). Alternative — flake input + overlay — is more moving parts for the
  same pin; use the flake input ONLY for importing the nixosModule into the deployment.
- Instance imports stay `Marmay.Auth.*` (already renamed in A5), so this step changes no
  Haskell source in competences beyond deletions.

**B3. Verify + close out.**
- Full browser flow re-test against the extracted service.
- Update teams-integration-plan.md: point the remaining Stage 1 infra items and all Stage 2
  service work (`/auth/teams/exchange`, JWKS, `/teams/config`) at the marmay-auth repo —
  **Stage 2 lands there, not here.**
- Remove the bootstrap-move follow-up note from the teams plan (done by A2/B1).

## Long-term consequences (accepted by this design)

- **Two-repo protocol coupling.** Claim shape, error codes, and bootstrap contract now span
  repos. Mitigation: consumers compile against the same library that mints — protocol
  agreement is type-checked at build time, not hoped for at runtime. The cost: a protocol
  change is a marmay-auth release + a pin bump in every consumer. Keep the pin a tag, bump
  deliberately.
- **The service stays a pure identity provider.** Nothing in marmay-auth may ever import a
  consumer's domain types (assertion carries `Text` email + name only). This boundary is
  what makes CMS reuse free — guard it in review.
- **The CMS consumer recipe** (documents itself in marmay-auth's README): depend on the
  library; hold `VerifierConfig`; implement your own `/api/login`-equivalent (validate →
  `consumeAssertionId` → your user lookup → your session mechanism); compose
  `runAuthBootstrap` with your hooks. Remember the recorded risk: on a shared trust domain,
  XSS in any consumer's user-generated content is auth-critical (teams plan, Risks).
- **GHC/toolchain lockstep.** marmay-auth must stay buildable with each consumer's GHC.
  With one maintainer this is trivial; it becomes the first friction point if the repos
  ever drift — accept until it hurts.
