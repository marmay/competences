# Microsoft Teams Integration + Shared Auth Service

## Context

The app should surface inside Microsoft Teams so students reach it where they already work — as convenient as the built-in grades tool. Decided: one Teams app, **configurable channel tab** per class Team pointing at that class's instance; **full app** in the tab (no trimmed views yet; mobile evaluated empirically). Built **together with the already-planned shared OAuth callback service** (docs/TODO.md "Shared OAuth callback service", before school year 2026/27), because a Teams app manifest binds to exactly one AAD app registration — the same consolidation the shared service needs. Markus is the M365 tenant admin, so app registration, admin consent, and org-catalog upload are self-service.

This plan is implementation guidance to code from, staged into independently shippable pieces.

## Progress (rough overview, updated 2026-07-23)

Legend: `[x]` done · `[~]` in progress · `[ ]` not started.

**Stage 1 — Shared auth service, browser flow** (~65%)

**Tree state (2026-07-23):** all of this is still uncommitted (staged + working tree); builds
clean. The instance side of the new login flow is DONE (`/api/login` incl. replay protection);
the shell bootstrap and the auth-service handlers are the remaining functional gaps — until
they exist, login is disconnected (shell embeds no JWT). `flake.lock` unstaged (toolchain now
GHC 9.14.1).

Refactor groundwork:
- [x] `Competences.Auth.*` namespace; one-way `Backend → Auth` dependency holds
- [x] Executable split (`app-auth/`, `app-backend/`; old `app/Main.hs` removed)
- [x] Shell generation extracted to `Backend/Shell.hs`; params collapsed into `ShellConfig`
      (its `returnUrl` field is now vestigial — remove with the bootstrap work)
- [x] OAuth2 config moved out of `Backend.HTTP` (`Auth/OAuth2Config.hs`)
- [x] Config split: `Internal/SecurityConfig` (shared loader + file-permission checks),
      `Auth/SecurityConfig` (`oauth2Config` + `authIssuerJwk`), `Backend/SecurityConfig`
      (`sessionIssuerJwk` only); old `Backend/Config.hs` deleted
- [x] Toolchain: `crypton` (was cryptonite), `jose >= 0.13`; `jose-jwt` dropped
- [x] `AuthUser` token projection (tripwire in place before `User` grows)
- [x] `Backend/API/Auth.hs` stub deleted
- [~] `Auth/Microsoft`: `getUserInfo` + `Office365User` folded in; still two calls
      (collapse to `exchangeCode -> Office365User`); still `newTlsManager` per request
- [ ] Drop `jwt` from build-depends (no module imports Web.JWT anymore — just the cabal line)

Session token (jose): [x] **done** — `generateJWT'`/`validateJWT'` on jose (HS256 via
`bestJWSAlg` on the oct JWK, `iss`+`aud` checked); `WebSocket.hs` consumes
`validateJWT' -> AuthUser`. Only unused-import Werror cleanup left in `Backend/Auth.hs`.

Identity assertion (`Auth/Assertion`):
- [x] Module complete (2026-07-22): alg-pinned, `aud` + `iss` checks, 10s skew; claims `sub` =
      o365 email, `jti`, and `name` (namespaced claim via `JOSEAssertion` `HasClaimsSet`
      wrapper — `name` kept for future auto-provisioning consumers, e.g. the CMS); `runJOSE`
      IO wrappers mirror `Backend/Auth.hs` (prime variants = compact `Text` form)
- [x] Single-use consumed-`jti` set (2026-07-23): `RestState.consumedAssertionIds` in
      `Backend/State.hs`; `ensureUnconsumed` prunes + checks + inserts in one STM transaction;
      retention = `exp` + skew (validator returns `validUntil` with skew already added).
      TODO: unit test — the failure mode (inverted prune) is invisible in manual testing
- [x] Instance `POST /api/login` (2026-07-23): validate (sig, `iss`, `aud` = configured
      `origin`, `exp` + configurable skew) → `jti` replay check → `findUserByOffice365Id`
      (new, in `Query.User`) → `toAuthUser` → `generateJWT'`; 403/500 error paths.
      Still open here: machine-readable error codes (bootstrap must distinguish
      invalid-assertion → silent re-auth from unknown-user → "no account" panel) — do
      together with the bootstrap
- [x] Instance config: `Backend/SecurityConfig` now has `authPublicKey`,
      `allowedExpirySkewDuration`, `origin` (NOTE: `aud` is exact-URI equality — auth
      service's `aud` and instance `origin` must match byte-for-byte, mind trailing slashes)

Auth service + instance wiring:
- [~] `app-auth/Main.hs`: CLI parser (`--port`, `--config`) done; no server behind it yet
- [~] `Auth/HTTP.hs`: empty module; old instance OAuth handlers kept as commented reference
- [ ] `/auth/login` (return-domain validation, CSRF + return cookies, 302 to AAD)
- [ ] `/auth/callback` (state check, code exchange, issue 60s assertion, `302 <return>#itoken`)
- [x] `/app/*` always serves shell without embedded JWT (old OAuth routes/cookies deleted from
      `Backend/HTTP.hs`)
- [ ] Bootstrap script in shell (fragment → `/api/login` → sessionStorage → `window.COMPETENCES_JWT`)
- [ ] Instance config: `authBaseUrl` (for the bootstrap redirect; `Nothing` = dev mode);
      keygen story for the auth-service Ed25519 JWK

Infra:
- [ ] Nix: `competences-auth` package; `authService` unit; shared-domain vhost; `/auth/` proxy
      on instance vhosts (cabal exe exists; nothing on the nix side)
- [ ] Azure: collapse to one app registration; delete per-class apps

**Stage 2 — Teams SSO + shell + CSP headers** — [ ] not started (gate first: verify `getAuthToken` from a subdomain)

**Stage 3 — Manifest, config page, org catalog, pilot** — [ ] not started

**Stage 4 — Mobile evaluation** — [ ] not started

## Target architecture

```
mathe.example.com                      (shared domain, nginx vhost)
  /auth/login                          browser flow entry (?return=<instance url>)
  /auth/callback                       the ONE AAD redirect URI for the tenant
  /auth/teams/exchange                 POST: AAD token from Teams SSO -> identity assertion
  /teams/config                        Teams tab configuration page (class picker)
                                       all served by new `competences-auth` executable

9a.mathe.example.com                   (per instance)
  /app/*                               serves shell unconditionally; client-side token bootstrap
  /teams                               Teams tab entry shell (teams-js init + SSO exchange)
  /api/login                           POST: identity assertion -> instance-minted 24h JWT
  /auth/*                              nginx-proxied to competences-auth (same-origin, no CORS)
  WebSocket                            unchanged: validates the instance's own HS256 JWT
```

Two token types: a **~60 s single-use identity assertion** (Ed25519-signed by the auth service; claims: email, name, `aud` = instance origin, `jti` = random nonce) and the **instance session JWT** (unchanged from today: HS256, 24h, uuid/role claims, minted by `generateJWT`). The auth service is a pure identity provider; each instance remains the sole session authority for itself. No flow contains a forwarding step — the client is at the correct instance before authentication begins, which is why the auth service needs no instance registry (only a return-domain pattern check).

## Login flows

1. **Browser, first visit**: instance serves shell → bootstrap finds no token → navigate to `/auth/login?return=<here>` → auth service validates return domain, cookies (CSRF state + return URL), 302 to AAD → AAD (usually silent SSO) → `/auth/callback` checks state, exchanges code, Graph `/me` → 60 s assertion, `302 <return>#itoken=…` → bootstrap scrubs fragment, `POST /api/login` → instance verifies assertion (sig/exp/aud), `findUserByEmail`, mints 24h session JWT → sessionStorage → `window.COMPETENCES_JWT` → app starts.
2. **Browser, reload within 24h**: bootstrap finds unexpired session JWT in sessionStorage → app starts. No auth service, no AAD contact.
3. **Teams tab**: Teams iframes `https://<class>.…/teams` directly (never the auth service). Shell: teams-js `initialize()` → cached session JWT? else `getAuthToken()` (silent, admin pre-consent) → `POST /auth/teams/exchange` (same-origin via instance nginx proxy) → assertion → `POST /api/login` → session JWT → app starts in iframe.
4. **24h expiry mid-session**: WS auth fails → `AuthenticationException` → navigate to `/teams` (in Teams; silent re-run of flow 3) or `/app/grid` (browser; flow 1, usually silent via AAD SSO). ~One reload per day per tab.

## Resolved decisions

1. **Auth service = second executable in the backend package** (`executable competences-auth` in `backend/competences-backend.cabal`), reusing `Auth.hs` (`exchangeCodeForToken`, `getUserInfo`). A separate cabal package buys nothing — same trust domain, same flake. (It links unused deps like postgresql-simple; cosmetic.)
2. **Front-channel exchange with a short-lived single-use identity assertion** — supersedes the shared-HMAC idea, the identity-JWT-as-session draft, and the redeem-code sketch in `backend/lib/Competences/Backend/API/Auth.hs` (delete that stub). The auth service signs a **~60 s assertion** (Ed25519 via `jose`; claims: `iss="competences-auth"`, `sub=<email>`, `name`, `aud=<instance origin>`, `jti=<random nonce>`, `iat`, `exp=now+60s`) and hands it to the *client*; the client exchanges it at the instance's new **public `POST /api/login`** for the instance's own 24h HS256 session JWT, minted by the existing `generateJWT` (Auth.hs:129-150) after `findUserByEmail`. Rationale (Markus's): the AAD authorization code already *is* the redeem layer (no extra redeem-code); asymmetric keys mean instances can verify identity but never mint it; the instance stays the sole session authority; and since the auth service must validate the return-URL domain anyway, redirecting to the instance is strictly less machinery than a back-channel call (no instance registry, no internal endpoint, no caller identification). `aud` check at `/api/login` prevents replaying a 9a assertion at 9b within its 60 s. Allow ~60 s clock skew (same host today).
   - **Single-use (replay window closure)**: the OAuth code was one-shot (AAD-enforced); a bare time-boxed assertion is not, so within its ~60 s life a captured copy could be redeemed at the matching `aud`. `/api/login` keeps a small in-memory consumed-`jti` set (TTL = exp + skew ≈ 120 s, then prune) and rejects repeats — restoring exactly-once. Per-instance, no coordination (`aud` pins the assertion to one instance; lost on restart, negligible). Same handling covers the Teams path (its `/auth/teams/exchange` output is redeemed at the same `/api/login`). Caveats: does **not** mitigate XSS (an attacker running script reads the session JWT from sessionStorage regardless — separate layer); reject-on-reuse means a dropped `/api/login` response forces a cheap silent re-auth rather than a retry (accepted, vs. caching the mint per `jti`).
3. **WebSocket auth is untouched.** The session JWT format, `extractUserFromJWT`, and the per-instance HS256 secret stay exactly as today — zero migration. Existing tokens keep working through the cutover. Consequence accepted consciously: role changes/user removal still take effect at token expiry (≤24h), as today. Optional future hardening, fully orthogonal: re-resolve via `findUserByEmail` at WS connect, demoting JWT claims to UI convenience. Record in TODO.md, don't build now.
4. **Assertion delivery via URL fragment + client-side bootstrap** (`#itoken=`, scrubbed via `history.replaceState`). The bootstrap exchanges it at `/api/login` and caches the **session JWT** in sessionStorage; only the 60 s assertion ever transits the front channel — the 24h token is born at the instance origin and never leaves it. One bootstrap contract for all entry paths (browser login, reload, Teams).
   - **Why this over a server-side `/app` design** (considered and rejected): three concerns turn out to be independent — log hygiene, reload-skip, and server-vs-client mint. Log hygiene is achievable two ways (fragment read by JS, *or* OIDC-style `form_post` with the assertion in a POST body — both keep it out of access logs / history / `Referer`; only a query string leaks it). So log hygiene alone does **not** force client-side handling. What separates the designs is the *reload* story: (a) **fragment + sessionStorage** (chosen) → reload is instant and local, no cookie, tab-close lifetime, costs the bootstrap + `/api/login`; (b) **form_post + cookie (PRG)** → server-side mint, reload-skip via cookie, but reintroduces ambient cookie authority + a latent CSRF surface for any future state-changing HTTP route + browser-close lifetime; (c) **form_post + embed, no storage** → simplest client but re-auths through AAD on *every* reload (latency + AAD load + prompt-risk if AAD's session lapses) plus the POST-resubmit wart. Since all state mutation goes over the WebSocket (which carries the JWT explicitly), a session cookie buys nothing and only adds CSRF surface — so (a) wins. Note: a non-Microsoft `form_post` interstitial still needs a one-line auto-submit (HTTP redirects can't synthesize a cross-origin POST), i.e. the client is unavoidably involved either way.
5. **Teams consent fallback**: error panel + retry only; admin consent makes the `authentication.authenticate()` popup unnecessary for tenant users. Add the popup later only if the pilot demands it.
6. **Config page instance list**: rendered by the auth service from its config (which nix/module.nix generates from the instance set) — keeps "new class = one nix attr".
7. **`frame-ancestors` allowlist config-driven**, not hardcoded (Microsoft domain list churns).

---

## Stage 1 — Shared auth service, browser flow only

Independently valuable: new class = one nix attr; per-class Azure app registrations get deleted.

### Auth service (new)

- `backend/app-auth/Main.hs` — CLI `--port`, `--config`.
- `backend/lib/Competences/Backend/AuthService.hs` — servant API + server:
  - `GET /auth/login?return=<url>`: validate `return` strictly (only `https://<label>.<allowedReturnDomain>/app/...`; port `validateReturnUrl` HTTP.hs:299-305 — open-redirect protection is now cross-origin-critical). Set CSRF state + return-URL cookies (port from `appCatchAllHandler` HTTP.hs:158-198; add `SameSite=Lax`, currently missing). 302 to AAD authorize.
  - `GET /auth/callback`: port `oauthCallbackHandler` (HTTP.hs:218-277): state check, code exchange (`Auth.hs:82-95`), Graph `/me` (`Auth.hs:113-126`), email = `mail` ?? `userPrincipalName` (HTTP.hs:250-252). Issue the 60 s identity assertion with `aud` = origin of the validated return URL → `302 <return>#itoken=<assertion>`.
  - Cleanup note (not Stage 1): scopes already include `openid profile email` — decoding the OIDC `id_token` could replace the Graph call.
- `backend/lib/Competences/Backend/AuthService/Config.hs` — JSON config: `signingKey` (Ed25519 private, agenix), `oauth2 {clientId, clientSecret, tenantId, redirectUri}`, `allowedReturnDomain`, `instances [{name, url}]` (used in Stage 3), later `teams` block.
- New module (in common or backend lib) for the assertion format: `generateIdentityAssertion` / `verifyIdentityAssertion` using `jose` (Ed25519, `aud` + `exp` checks with ~60 s skew). Add `jose` to cabal deps now (needed for AAD validation in Stage 2 anyway). Keygen documented: `openssl genpkey -algorithm ed25519` or a tiny `--gen-key` mode.

### Instance changes

- `backend/lib/Competences/Backend/Config.hs`: `oauth2` becomes optional/ignored (drop next release); add `authBaseUrl :: Maybe Text` (Nothing = dev mode, no redirect) and `authPublicKey` (plain config, not secret).
- `backend/lib/Competences/Backend/HTTP.hs`:
  - Delete `oauthCallbackHandler`, cookie helpers, `getAuthorizationUrlWithState`; remove `oauth/callback` from `AppAPI` (81-104).
  - New endpoint `POST /api/login` in `AppAPI`: body = identity assertion → `verifyIdentityAssertion` (Ed25519 via `authPublicKey`, `exp`, `aud == own origin`) → `findUserByEmail` (already in HTTP.hs:308-312, stays put) → existing `generateJWT` → JSON `{jwt}`. Unknown email → 403 with a machine-readable code (the bootstrap renders a human-readable "no account in this class" panel). This endpoint is safely public: the assertion signature is the protection.
  - `/app/*` always serves the shell (`renderFrontendHTML`, 331-376) **without** server-embedded JWT. New inline bootstrap script placed before the index.js module script (module scripts defer, so ordering holds):
    1. `#itoken` in fragment → `history.replaceState` scrub → `POST /api/login` → store returned session JWT in sessionStorage.
    2. Else read sessionStorage token; **client-side `exp` check** (base64-decode payload; without this an expired cached token loops forever).
    3. Valid session JWT → `window.COMPETENCES_JWT = token` (frontend reads exactly this: `frontend/lib/Competences/Frontend/WebSocket.hs:40`, `frontend/app/Main.hs:42`). Invalid/absent → `location.href = authBaseUrl + "/auth/login?return=" + encodeURIComponent(location.href)`; if `authBaseUrl` unset, do nothing (preserves disconnected dev mode, Main.hs:44-55).
- `backend/lib/Competences/Backend/WebSocket.hs`: **no changes.**
- `backend/app/Main.hs`: thread new config (149, 266).
- Delete `backend/lib/Competences/Backend/API/Auth.hs`.

### Nix

- `nix/backend.nix` / `flake.nix`: package `competences-auth` (mirror existing exe packaging, flake.nix ~124-153).
- `nix/module.nix`: `services.competences.authService = { enable, port, secretsFile }` + systemd unit (mirror instance unit 288-356, minus DB/CAS); nginx vhost for bare shared domain → `/auth/`; add `location /auth/ { proxy_pass ...authPort; }` to every instance vhost (367-394) — makes the Teams exchange same-origin later, zero CORS. Document `/auth/` as reserved path. Generate auth-service instance list from the instances attrset. Instance secrets shrink to `{jwtSecret-legacy?, authBaseUrl, authPublicKey}` — public key can move to plain module config.
- Azure: afterwards delete per-class app registrations; one registration, one redirect URI remains.

### Auth-layer refactor closeout (do before finishing the Teams integration)

Emerged while migrating the session token to `jose` (HS256 stays — wire-compatible swap, not a token change):

- **`AuthUser` token projection** (added 2026-06-14). The session JWT currently embeds the whole `User` under the private claim `https://bu-ki.at/#user`. `User` happens to be exactly `{id, role, name, o365Id}` today, so this is already minimal — but `toJSON user` means any *future* `User` field would flow into the client-readable token silently. Before finishing the Teams work, introduce a slim `AuthUser` projection (those four fields) so that extending `User` forces a conscious decision about what enters the token. The value is the indirection-as-tripwire, so it must exist **before** `User` is ever extended; until it lands, a comment at the embed site (`Backend/Auth.hs`, `UserClaims`/its `ToJSON`) should warn the next field-adder.
- **Session-token `jose` migration** (in flight): `generateJWT`/`validateJWT` move to `jose` — run in `runJOSE`, JWK-based, alg pinned via `bestJWSAlg`; `JWTSecret` (Text) becomes an `oct` `JWK` built from the config secret; `WebSocket.hs` consumes the new `validate → User` shape; drop the `jwt` package. This lightly revises decision 3's "WebSocket.hs: no changes" — *wire format and semantics are unchanged* (still HS256, same claims); only the library and the `validate` signature change.

### Stage 1 verification

Fresh browser profile: teacher + student login; deep link returns to the right page; reload skips AAD *and* the auth service (network tab: only the instance origin); expired sessionStorage token → clean silent re-login, no loop; tampered `return` (`https://evil.example`) rejected; state mismatch → 400; expired/tampered/wrong-`aud` assertion → 403 at `/api/login` (test wrong-`aud` with two instances); **replayed assertion (same `jti` twice) → 403 on the second attempt**; unknown email → "no account" panel; pre-cutover session JWTs keep working (format unchanged — nothing to migrate); tab close + reopen on a shared machine → no lingering session (sessionStorage gone).

---

## Stage 2 — Teams SSO + Teams shell + real CSP headers

### Gate first (one afternoon)

**Verify `getAuthToken` works from a subdomain page** whose AAD Application ID URI lives on the apex (`api://mathe.example.com/<clientId>` vs tab on `9a.mathe.example.com`). Teams' domain-matching rules for subdomains have churned across SDK generations. Throwaway sideloaded tab + minimal HTML page suffices. If it fails: set the ID URI to a dedicated host and/or fall back to popup auth — decide then, before building the rest.

### AAD app registration (tenant admin checklist)

1. Expose an API → Application ID URI `api://mathe.example.com/<clientId>`.
2. Scope `access_as_user` (admins-and-users), **grant tenant-wide admin consent** (kills the consent popup for all tenant users).
3. Authorized client applications for that scope (verify current list in MS docs "Register your tab app with Microsoft Entra ID" — it churns): `1fec8e78-bce4-4aaf-ab1b-5451cc387264` (Teams desktop/mobile), `5e3ce6c0-2b1f-4285-8d4b-75ee78787346` (Teams web), `4765445b-32c6-49b0-83e6-1d93765276ca` (M365 web), `0ec893e0-5785-4de6-99da-4ed124e5296c` (M365 desktop), `d3590ed6-52b3-4102-aeff-aad2292ab01c` (Outlook desktop), `bc59ab01-8403-45c6-8796-ac3ef710b3e3` (Outlook web).
4. AAD app manifest: `requestedAccessTokenVersion: 2` (deterministic v2 tokens).

### AAD token validation

- `backend/lib/Competences/Backend/AuthService/JWKS.hs`:
  - `fetchJWKS`: GET `https://login.microsoftonline.com/<tenantId>/discovery/v2.0/keys`; `jose`'s `JWKSet` parses it directly. Hoist ONE shared TLS `Manager` into the service env (Auth.hs currently does `newTlsManager` per request, lines 80/115 — fix while here).
  - Cache (`TVar` + timestamp): refetch on unknown `kid` (retry verify once) or age >24h; on refresh failure serve stale + log (AAD JWKS outage must not break logins on cached keys).
  - Validate: RS256 sig by `kid`; `aud` ∈ {bare clientId GUID, `api://…/<clientId>`} (accept both defensively); `iss` = `https://login.microsoftonline.com/<tenantId>/v2.0` (pins tenant); `exp`/`nbf` skew ~300s.
- `POST /auth/teams/exchange`: `{token}` → validate → email from `preferred_username` ?? `upn` ?? `email`, lowercase-normalized → issue the 60 s identity assertion (`aud` = instance origin, from the nginx-proxied `Host`/`X-Forwarded-Host` header, validated against `allowedReturnDomain`) → `{assertion}`; 401 with machine-readable error code otherwise.
- **Action item before relying on it**: confirm `mail == userPrincipalName` across the tenant (`/users?$select=mail,userPrincipalName`); if they diverge, normalize the browser flow to UPN too (one line in callback). Long-term-correct key is AAD `oid` — record in TODO.md, don't build now.

### CSP middleware

- New `backend/lib/Competences/Backend/Middleware.hs` using `wai-extra` `modifyResponse` (already a dep): per-path `frame-ancestors` —
  - `/teams`: configured allowlist, default `teams.microsoft.com *.teams.microsoft.com *.office.com *.microsoft365.com *.cloud.microsoft` (verify against MS docs "Content security policy for tabs" at implementation time; keep config-driven).
  - everything else: `frame-ancestors 'none'` (also covers the X-Frame-Options gap).
- Move the full CSP from the meta tag (HTTP.hs:318-328, injected at 338 — meta `frame-ancestors` is ignored anyway) into the header; delete the meta tag. Existing `connect-src 'self' ws: wss:` and `script-src 'self' …` already cover WebSocket, same-origin exchange, vendored teams-js.
- Wrap `httpApp` in `backend/app/Main.hs:266-271` (WS upgrades bypass it — fine). Same middleware on the auth service + `Cache-Control: no-store` on `/auth/*`.

### Teams shell

- Vendor `@microsoft/teams-js` v2.x (pinned, single file) at `frontend/static-src/teams-js/MicrosoftTeams.min.js`; add copy steps in `deploy_frontend.sh` and `nix/frontend.nix` installPhase (note: static blobs submodule is gone; static/ is assembled from static-src).
- New route `GET /teams` in `AppAPI` (before the `/app` catch-all): factor `renderFrontendHTML` so browser/Teams shells share head/hashes/MathJax and differ only in bootstrap. Teams bootstrap:
  1. load teams-js → `await microsoftTeams.app.initialize()`
  2. sessionStorage already has an unexpired *session* JWT? skip to 6.
  3. `getAuthToken()` → 4. `POST /auth/teams/exchange` (same-origin via nginx proxy) → assertion
  5. `POST /api/login` with the assertion → session JWT → sessionStorage
  6. `window.COMPETENCES_JWT = jwt; window.COMPETENCES_IN_TEAMS = true` → inject index.js module script → `app.notifySuccess()`.
  - Failure: human-readable panel + retry button (no popup fallback yet).
- `frontend/app/Main.hs:129-133` (`handleAuthFailure`) — **the only WASM change**: if `window.COMPETENCES_IN_TEAMS`, navigate to `/teams` (silent re-exchange; Teams caches AAD tokens) instead of `/app/grid` (which would eventually frame the AAD login page — forbidden in iframes). Once-a-day iframe reload is acceptable; no-reload refresh is future polish.
- Cheap robustness: `/app/*` bootstrap checks `window.self !== window.top` → bounce to `/teams`.

### Stage 2 verification

`curl -I` per path for headers; exchange endpoint against a real captured `getAuthToken` token + tampered + wrong-audience tokens; JWKS unknown-kid refresh path; manual `/teams` page in a sideloaded tab.

---

## Stage 3 — Manifest, config page, org catalog, pilot

- New `teams/` dir: `manifest.json` (schema ≥1.17: new GUID app id; `configurableTabs: [{configurationUrl: "https://mathe.example.com/teams/config", scopes: ["team"], canUpdateConfiguration: true}]`; `validDomains: ["mathe.example.com", "*.mathe.example.com"]` — wildcard excludes apex, need both; `webApplicationInfo: {id: <clientId>, resource: "api://mathe.example.com/<clientId>"}`), icons `color.png` 192×192 + `outline.png` 32×32 (derive from the SVG favicon, HTTP.hs:341-342), `package.sh` (zip).
- `GET /teams/config` on the auth service: `<select>` over configured instances; `pages.config.registerOnSaveHandler` → `setConfig({entityId: subdomain, contentUrl: https://<sub>.…/teams, websiteUrl: https://<sub>.…/app/grid, suggestedDisplayName})`; `setValidityState(true)` on selection. (`websiteUrl` = free "open in browser" escape hatch, incl. mobile.)
- Upload via Teams admin center → org catalog; optionally app-permission-policy so only teachers can *add* the tab. Pilot in ONE class Team ~2 weeks before adding to other Teams.

Verification: tab add/configure/rename/remove lifecycle; second class second Team; non-member opening tab → clean "no such user" panel; short-`exp` token in dev → silent re-auth reload inside Teams.

## Stage 4 — Mobile evaluation (empirical)

Teams iOS/Android webview with the 11 MB app.wasm + 1.8 MB MathJax: measure cold load + memory on a school-typical phone. Plain phone *browser* already runs the app very well, so the question is purely the Teams-webview wrapper. If unacceptable: short-term `app.getContext()` clientType check → "open in browser" interstitial; long-term the flag-trimmed ~5 MB overview build (explicitly deferred).

## Risks / long-term consequences

- **Email as the join key** crosses the auth boundary; alias/rename divergence between UPN and `mail` breaks Teams-vs-browser parity (see action item). Eventual fix: key users on AAD `oid`. → TODO.md.
- **Ed25519 keypair**: public key is config, private key one agenix secret; rotation = accept-two-keys window. Instances can verify, never mint.
- **Microsoft churn** lives in exactly three places, all config/docs not code: authorized-client-ID list, `frame-ancestors` domain list (config-driven), manifest schema version.
- **getAuthToken tokens ~1h** but exchanged once per load + once per 24h reload — no hot loop.
- **Guests/personal accounts** effectively unsupported (`#EXT#` UPNs won't match) — fine for a school tenant; document.
- **Local dev**: browser flow keeps working (`authBaseUrl = Nothing` → existing disconnected mode). Real Teams dev needs a public HTTPS tunnel + a *separate* dev AAD app + dev manifest sideloaded to a test Team (ID URI must match tunnel domain).
- **sessionStorage, deliberately not localStorage**: per-tab, dies on tab close — right default for shared school computers, and no logout function needed (parity with today). If ever upgraded to localStorage (persistent sessions across restarts), a **logout function becomes mandatory** (clear token; optionally AAD sign-out redirect). The AAD-SSO-silent re-login makes localStorage's benefit marginal, so this is not planned.
- **Doors deliberately left open, not built**: because instances mint and validate their own sessions, a per-instance revocation list and a token-renewal endpoint (extend/re-issue against a still-valid session, or silent re-run of the flow) are both purely instance-local additions later — no auth-service coordination needed. Likewise live re-resolution of user/role at WS connect (decision 3).
- **Auth service URL shape**: `auth.mathe.example.com` (subdomain) vs `mathe.example.com/auth/` (apex path) — equivalent; pick in the nix module. The structural requirement is only that each instance proxies `/auth/*` for the same-origin Teams exchange.

## Suggested release mapping

Stage 1 fits the existing "before school year 2026/27" shared-auth TODO (replace that TODO entry with a pointer to this plan). Stages 2-3 follow once Stage 1 has run quietly for a couple of weeks; Stage 4 during the pilot.
