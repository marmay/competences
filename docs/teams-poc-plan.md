# Teams PoC — Detailed Implementation Plan

Companion to [teams-integration-plan.md](teams-integration-plan.md) (architecture,
flows, rationale — read that first). This document is the file-by-file
implementation guide for the proof-of-concept: **the full app in a Teams channel
tab, deep-linked to the assignments page**. Markus implements from this plan;
Claude's role was the plan itself.

## Decisions recorded (2026-08-31)

1. **PoC content = full app, deep-linked.** The tab's `contentUrl` points at
   `/teams/assignments` (the assignments page already default-selects the newest
   HomeExercise via `Query.DefaultSelection.defaultAssignment`). No trimmed
   view, no new page. A homework-only page is an ordinary in-app feature for
   later; "more views" = more tabs with different `contentUrl`s.
2. **Teams bootstrap core lives in marmay-auth** (`Marmay.Auth.Bootstrap`),
   mirroring the existing `bootstrapCoreScript` / app-hooks split, so the CMS
   can reuse it. Cost accepted: pin-bump friction while iterating
   (`cabal.project.local` override during development).
3. **Dev/test topology: directly against production infra** (between school
   years, no active users). No tunnel, no separate dev AAD app. Consequence:
   `laxReturnUrlCheck` stays off; everything is tested under real HTTPS and the
   real tenant.
4. **Ownership**: Markus implements everything from this plan (Claude's
   drafts may be consulted); tenant-admin work (Azure, Teams admin center) is
   Markus's by necessity.
5. **(2026-09-01) Authorized clients: Teams-only for the PoC.** Only the Teams
   desktop/mobile and Teams web GUIDs are authorized on the scope. The
   M365/Outlook shell GUIDs are deferred until the app should run there
   (consequence: no silent SSO from microsoft365.com/Outlook — fine for a
   channel tab).
6. **(2026-09-01, superseded by decision 8) Identity key = lowercased UPN in
   both flows.** No user data
   exists yet, so instead of the 0.2 tenant scan the browser flow switches
   from `mail ?? userPrincipalName` to `userPrincipalName`, lowercased — same
   `T.toLower` as the Teams exchange (A.3 step 4). No migration needed.
   Long-term-correct key remains AAD `oid` (still a TODO, not PoC scope).
7. **(2026-09-01) Auth-host bounce architecture** — forced by the Phase 0
   gate result (exact origin match; Entra refuses wildcard ID URIs) and
   chosen over per-instance registrations (linear admin cost, undoes Stage 1
   consolidation). `getAuthToken` runs on a `/teams/sso` page on the auth
   host; the assertion travels to the instance via the browser flow's
   existing `#itoken=` fragment contract (fragment, not query: stays out of
   access logs and Referer; `bootstrapCoreScript` already scrubs it).
   Consequences: no teams-js on instances (B.1 vendoring dropped — the bounce
   page loads it from the Microsoft CDN), no `/auth/` nginx proxy (old B.6
   dropped — the exchange is same-origin on the auth host), and the instance
   Teams shell reuses `bootstrapCoreScript` with a configurable login route.
8. **(2026-09-01) id_token + oid-keyed identity — supersedes decision 6.**
   The browser flow reads identity from the **id_token** already returned by
   the code exchange (currently discarded) instead of calling Graph —
   `getUserInfo`/`Office365User`/`User.Read` are dropped, and both flows
   validate their AAD-issued token through the same validator and read the
   same claims. `IdentityAssertion` becomes
   `{ assertionId, oid, upn (lowercased), name }`: **`oid`** (the immutable
   Entra directory Object ID, tenant-wide, identical across consumers) is the
   identity key; **`upn`** is the human-readable matcher for provisioning.
   Consumers match by oid first, else by upn + backfill the oid on first
   login (lazy binding); provisioning stays address-based for teachers
   (`--ensure-teacher-o365` additionally accepts an oid — `@` vs UUID
   dispatch). UPN mutability (renames) thereby stops being load-bearing.
   Cheap now because no user data exists; both consumers (competences, CMS)
   adjust their `/api/login` matching in the same step.
9. **(2026-09-01) Frame-aware bootstrap core — no Teams-specific app
   surface.** Supersedes the Phase B parts of decision 7. The only real
   browser/Teams difference on the app side is the no-token redirect target
   (AAD refuses framing), and that is decidable at runtime:
   `bootstrapCoreScript`'s `loginUrl()` checks `window.self !== window.top`
   and targets `AUTH_BASE + /teams/sso?return=…` when framed, `/auth/login`
   otherwise. Consequences: every auth-library consumer (CMS included) is
   Teams-capable for free; competences drops the `/teams/*` routes, the
   ShellMode variant, the path mapping, and the WASM change (expiry reload
   lands in the shell, which re-runs the bounce — view resets to grid, same
   as the browser flow); Phase B shrinks to the CSP change (frame-ancestors
   allowlist on `/app/*`) plus the pin bump. Tab `contentUrl` remains the
   gate-proven bounce URL, so the in-tab-navigation bet covers only
   expiry/retry. A pasted `/app/…` URL as a tab now simply works.
10. **(2026-09-01) The Teams app is a "BG Horn" meta-app.** See the Phase C
    framing note: one catalog entry whose config page selects among ALL
    internal applications (`applications` registry replaces the
    competences-only `instances` list); competences classes are just the
    first entries.

**Precondition**: the Stage 1 production cutover (marmay-auth deployed, one
Azure app registration, instances on the assertion login flow) must be live
first — the Teams flow reuses `/api/login`, the Ed25519 keys, and the nginx
layout. This folds naturally into the current infra setup.

## Flow being built (recap) — auth-host bounce (rev. 2026-09-01)

Phase 0 established: Teams requires the SSO resource authority to **exactly
match the iframe origin** (no apex coverage, Entra refuses wildcards). So
`getAuthToken` runs on the auth host, and the result rides the browser flow's
existing `#itoken=` fragment contract to the instance. This is the browser
flow with one substitution: `getAuthToken`+JWKS-validation replaces the AAD
authorize-code roundtrip.

```
Teams iframes https://auth.<apex>/teams/sso?return=https://<class>.<apex>/app/assignments
  bounce page (marmay-auth, server-validated return):
    teams-js initialize() → getAuthToken()      (silent; admin pre-consent;
                                                 origin = auth host = ID URI host)
    → POST /auth/teams/exchange?return=…        (same-origin, raw AAD token)
        validate AAD token via tenant JWKS → 60 s identity assertion,
        aud = return-URL origin (same derivation as the browser callback)
    → location.replace(return + '#itoken=' + assertion)
  instance /app/<view> — the ORDINARY shell (decision 9, NO teams-js,
  no Teams-specific route):
    runAuthBootstrap (frame-aware core):
      #itoken fragment → scrub → POST /api/login → session JWT → sessionStorage
      (cached usable JWT → skip everything; no usable token → framed ?
       bounce to /teams/sso : redirect to /auth/login)
  WebSocket auth: completely unchanged
```

Expiry (24 h) inside Teams: WASM `handleAuthFailure` reloads `/app/grid`
(unchanged) → shell finds no usable JWT, detects the frame → bounces to
`/teams/sso` → silent re-auth → back with a fresh assertion. Same loop as
the browser flow (including the view reset to grid).

---

## Phase 0 — Gate + Azure registration

Do this before writing any Haskell. Total: one afternoon.

### 0.1 AAD app registration (tenant admin) — DONE 2026-09-01

On the ONE consolidated app registration (the Stage 1 one). Note: currently
registered against the test domain; on cutover to the production apex, update
the ID URI, the Teams manifest (`webApplicationInfo.resource`, `validDomains`,
`contentUrl`s), and marmay-auth's `applicationIdUri` — admin consent survives
the URI change (grants attach to the service principal + scope, not the URI
string).

1. Expose an API → Application ID URI `api://<apex-domain>/<clientId>`
   (apex = the shared domain, e.g. `mathe.example.com`).
2. Add scope `access_as_user` (admins-and-users) and grant **tenant-wide admin
   consent** (this is what makes `getAuthToken` silent for all tenant users).
3. Authorized client applications for that scope — added: `1fec8e78-…` (Teams
   desktop/mobile), `5e3ce6c0-…` (Teams web). Deferred (decision 5): M365
   web/desktop, Outlook desktop/web — full GUIDs in teams-integration-plan.md
   Stage 2; verify against current MS docs when adding.
4. AAD app manifest: `requestedAccessTokenVersion: 2`.

### 0.2 Identity pre-flight — RESOLVED 2026-09-01 (decision 6)

Original risk: the browser flow keyed users on `mail ?? userPrincipalName`
(set at assertion minting, `Marmay.Auth.HTTP.mkIdentityAssertion`); the Teams
exchange only has the AAD token, whose `preferred_username` is normally the
UPN — **no Graph call is possible there** (the token's audience is our API,
not Graph; avoiding OBO is the point of the design). `mail ≠
userPrincipalName` would have split a user into two `office365Id`s.

Resolution: no user data exists yet, so the tenant scan is unnecessary —
**normalize the browser flow to lowercased UPN now** (drop the `mail ??`
preference in `mkIdentityAssertion`, apply `T.toLower`; the Teams exchange
applies the same lowercasing per A.3 step 4). Folds into the Phase A
marmay-auth work. Long-term-correct key is AAD `oid`; stays a TODO, not PoC
scope.

### 0.3 The subdomain gate test

Verify `getAuthToken` works from a **subdomain** page while the Application ID
URI lives on the **apex** — Teams' domain-matching rules for this have churned
across SDK generations. If it fails, the ID URI moves (e.g. to a dedicated
host) and the manifest's `webApplicationInfo.resource` changes — decide then,
before any backend code exists.

Cheapest possible rig, zero repo changes:

- Hand-write `teams-gate.html`: loads teams-js 2.x (Microsoft CDN is fine for a
  throwaway), `app.initialize()`, `authentication.getAuthToken()`, then prints
  the decoded payload claims (`aud`, `iss`, `preferred_username`, `name`,
  `tid`) **and the raw token** into the page. The raw token is deliberately
  displayed: it doubles as the test input for the Phase A exchange endpoint
  (curl it before any Teams UI exists).
- Drop the file into the deployed `staticDir` on the server
  (`/var/lib/competences/static/`) — `serveDirectoryWebApp` serves it at
  `https://<class>.<domain>/static/teams-gate.html` with no CSP header (the
  current CSP is a meta tag on shell pages only), so it is frameable today.
- Sideload manifest: minimal `staticTabs` (personal scope) with that URL as
  `contentUrl`, `validDomains: ["<apex>", "*.<apex>"]`, `webApplicationInfo`
  = {clientId, `api://<apex>/<clientId>`}. Upload as custom app for yourself
  in the Teams admin center (custom app upload must be permitted).

Gate passes when the page shows claims with `aud` = your ID URI (or the bare
clientId GUID — note which!) without any popup. Keep the sideloaded gate app
around; it regenerates fresh test tokens on demand throughout Phase A.

**Result (2026-09-01, test domain bu-ki.at): FAILED as anticipated.**
`getAuthToken` → "App resource defined in manifest and iframe origin do not
match": apex ID URI (`api://bu-ki.at/<clientId>`) is not accepted for a page
on `m2a.bu-ki.at` — the current Teams build requires the resource authority to
match the iframe origin. Follow-up experiments before any redesign:

1. Wildcard URI `api://*.bu-ki.at/<clientId>` — **REFUSED by Entra**
   (2026-09-01, portal validation: "Enthält keine Platzhalterzeichen").
   Dead end; Teams never got asked.
2. Exact `api://m2a.bu-ki.at/<clientId>` — **GREEN** (2026-09-01): silent
   token, no popup. Rule confirmed: exact host match between resource
   authority and iframe origin.

Findings from the green run (test tenant, real token):
- `aud` = **bare clientId GUID** (`ver: 2.0`) — this is what the accepted-
  audience set actually needs; the `api://…` form did not arrive. The GUID is
  also stable under ID-URI changes.
- `iss` = `https://login.microsoftonline.com/<tid>/v2.0` — matches A.2's
  byte-exact pin. `alg` = RS256.
- `upn`/`email` **absent** (optional claims in v2 tokens); `preferred_username`
  present and carries the UPN — the A.3 identity chain resolves on its first
  link. `oid` present (future UPN→oid migration has its source on this path).
- Scope is named `bghorn-app` in this tenant (not the docs-conventional
  `access_as_user`) — works fine; relevant only if code ever inspects `scp`.

**Consequence: auth-host bounce is the design** (exact match + Entra's
wildcard refusal rule out apex/wildcard URIs; per-instance registrations
rejected — linear admin cost per class, undoes Stage 1 consolidation).
`contentUrl` moves to the auth service host, ID URI becomes
`api://auth.<apex>/<clientId>`; a bounce page there does getAuthToken +
exchange, then `location.replace` to the instance (validated `return` param,
existing `isAllowedReturnUrl` trust model) with the assertion; the instance
redeems at `/api/login`. Same handoff shape as the browser flow; keeps one
registration; A.4's "never navigates" relaxes to "never navigates to AAD".
Phases A–C below have been rewritten for this architecture (2026-09-01,
decision 7).

Re-upload note: bump manifest `version` each time; propagation is slow —
remove + re-add the app or restart the client.

---

## Phase A — marmay-auth: Teams SSO exchange

All in `~/devel/hs/marmay-auth`. Consumed by competences via pin bump at the
end (during development: `cabal.project.local` with a local
`source-repository-package` / `packages:` override).

Module layout (decided with decision 8; revised 2026-09-01 — splitting the
existing browser handlers out of HTTP.hs was deprioritized; only the new
Teams routes get their own module):

```
Marmay/Auth/HTTP.hs                        -- browser flow (/auth/login, /auth/callback),
                                           -- AuthEnv, shared back half (identity →
                                           -- assertion, isAllowedReturnUrl, audience
                                           -- derivation, #itoken contract)
Marmay/Auth/HTTP/Teams.hs                  -- NEW: /teams/sso, /auth/teams/exchange
                                           -- (+ /teams/config in C.1)
Marmay/Auth/Microsoft/CodeExchange.hs      -- token-endpoint POST, returns the id_token
Marmay/Auth/Microsoft/AuthTokenValidator.hs -- JWKS fetch/cache + AAD token validation
                                           -- (A.2); consumed by BOTH flows
```

### A.0 Identity rework (decision 8) — do first, both flows build on it

- `IdentityAssertion` (Assertion.hs) becomes
  `{ assertionId, oid :: Text, upn :: Text, name :: Text }` (upn lowercased
  at minting). `oid` is the identity key; `upn` the provisioning matcher.
- Browser flow: `exchangeCodeForToken` returns the **`id_token`** field
  (today it extracts `access_token` and discards the id_token);
  `getUserInfo`, `Office365User`, and the Graph call are deleted; `User.Read`
  drops out of the authorize scope (`openid profile email` remain). The
  id_token is validated through A.2's validator (same JWKS, same
  `{clientId}` audience, same issuer pin) — one hardened path for both
  flows, and identity extraction (`oid`/`preferred_username`/`name`) is the
  same code as the exchange's.
- Consumers (competences `/api/login`, CMS): match by `oid` first; else by
  `upn`, then **backfill the oid** (lazy binding on first login); else
  `unknown-user`. Teacher-driven user creation stays address-based;
  `--ensure-teacher-o365` accepts either form (`@` → upn, UUID → oid).
- This touches the live Stage-1 browser flow: own commit, manual browser
  login against CMS + competences before moving on.

### A.1 Config: `TeamsConfig` (SecurityConfig.hs)

Add to `SecurityConfig`:

- `teamsConfig :: !TeamsConfig` — **mandatory** (implemented 2026-09-01,
  commit 52f5e3b): Teams login is a core capability of the service, there is
  no disabled mode. The lazy JWKS cache keeps always-on free at startup. The
  JSON key (`"teamsConfig"`) defaults when absent, so existing deployed
  configs keep parsing — nothing to add at the A.7 redeploy unless an
  `applicationIdUri` is wanted.
- `data TeamsConfig = TeamsConfig { applicationIdUri :: !(Maybe Text) }` —
  optional extra accepted audience. The gate test showed v2 tokens arrive
  with `aud` = **bare clientId GUID**, so the accepted-audience set is
  `{clientId}` ∪ `applicationIdUri` (defensive; `tenantId`/`clientId` come
  from `oauth2Config`).

### A.2 Token validation: new module `Marmay.Auth.Microsoft.AuthTokenValidator`

State: `data JWKSCache = JWKSCache { keys :: TVar (Maybe (JWKSet, UTCTime)), … }`
holding the shared `Manager` and the discovery URL
`https://login.microsoftonline.com/<tenantId>/discovery/v2.0/keys`.
Create in `app/Main.hs` next to the `Manager` and thread into `authServer`
(extend its signature; consider bundling `Manager + SecurityConfig + JWKSCache`
into a small `AuthEnv` record while you're there — three positional params is
the threshold where I'd stop threading them separately).

Exports (suggested signatures):

- `mkJWKSCache :: Manager -> Text {- tenantId -} -> IO JWKSCache` — no fetch
  yet (lazy; the service must start when AAD is unreachable).
- `getJWKS :: JWKSCache -> IO (Either Text JWKSet)` — cached if age < 24 h;
  else refetch; **on refetch failure serve stale + log** (an AAD JWKS outage
  must not break logins that cached keys can still validate). `jose`'s
  `JWKSet` has a `FromJSON` instance that parses the discovery document
  directly.
- `validateEntraToken :: JWKSCache -> SecurityConfig -> TeamsConfig
     -> BL.ByteString -> IO (Either Text EntraClaims)`
  - `decodeCompact`, then verify with the `JWKSet` as the key argument —
    jose's `VerificationKeyStore` instance for `JWKSet` selects candidate keys
    by `kid`/`alg` automatically; no hand-rolled kid lookup.
  - Validation settings, mirroring the style of `Marmay.Auth.Assertion`:
    pin `RS256` (`validationSettingsAlgorithms`), issuer predicate
    `== "https://login.microsoftonline.com/<tenantId>/v2.0"` (byte-exact; pins
    the tenant), audience predicate: membership in the accepted-audience set,
    allowed skew ~300 s.
  - **Unknown-`kid` path**: on signature/key-not-found failure, force one
    refetch and retry once (covers Microsoft's key rotation). Guard the
    forced refetch with a minimum interval (~1 min, timestamp in the cache):
    without it, garbage tokens with random `kid`s can each trigger a request
    to Microsoft; with it, they only cost local CPU.
- `data EntraClaims` — follow the `JOSEAssertion` pattern exactly (wrap
  `ClaimsSet` in a record with a `HasClaimsSet` instance + `FromJSON` for the
  extra claims; do **not** use the deprecated `unregisteredClaims`/`addClaim`):
  fields for `oid` (`Text` — required, the identity key per decision 8) and
  `preferred_username`, `upn`, `email`, `name` (all `Maybe Text`) — use
  jose's `verifyJWT` (the `HasClaimsSet`-polymorphic verifier) so the custom
  claims come out of validation directly. Consumed by BOTH flows: the Teams
  exchange validates the `getAuthToken` token, the browser flow validates
  the id_token (A.0) — same accepted audiences, same issuer pin, same
  identity extraction.

### A.3 Exchange endpoint: `POST /auth/teams/exchange` (HTTP.hs)

Extend `AuthAPI` under the existing `"auth" :>` prefix:

```
"teams" :> "exchange"
  :> QueryParam "return" Text               -- full instance return URL
  :> ReqBody '[OctetStream] BL.ByteString   -- the raw getAuthToken token
  :> Post '[JSON] ExchangeResponse
```

The caller is the same-origin bounce page (A.4), which passes its own
`return` query param straight through. The instance origin comes from that
URL — **not** from the Host header (the request legitimately arrives at the
auth host now).

Handler behavior, in order:

1. Missing/unparseable `return` (`parseAbsoluteURI`) → 400
   `{error: "invalid-return"}`.
2. **`isAllowedReturnUrl`** on it — the identical check and trust model as
   `loginHandler`/`callbackHandler`. Reject → 403
   `{error: "disallowed-return"}`. This is what keeps a directly-addressed
   request from minting an assertion for an arbitrary origin. (Defense in
   depth — an assertion for a non-trust-domain `aud` is redeemable nowhere,
   but don't rely on that alone.)
3. `validateEntraToken` → failure → 401 `{error: "invalid-token", message}`
   (include the jose error text; opaque auth errors cost hours — cf. the
   AADSTS7000222 incident).
4. Identity (decision 8, shared extraction code with the browser flow):
   `oid` from the claims (required — validation already guaranteed it);
   `upn` = `preferred_username ?? upn ?? email`, **lowercased** (gate data:
   only `preferred_username` is present in this tenant's v2 tokens and it
   carries the UPN; the fallbacks are for claim-shape drift); `name` = the
   `name` claim, falling back to the upn.
5. Build `IdentityAssertion` (fresh `assertionId`); audience = the return URL
   with `uriPath`/`uriQuery`/`uriFragment` cleared — **byte-identical
   derivation to `callbackHandler`'s `mintedTokenAudience`; factor it into a
   shared helper** so the two flows can't drift. `generateIdentityAssertion'`
   → 200 `{assertion: <compact text>}`.

Errors use the `{error, message}` JSON contract (same client-side dispatch as
`/api/login` errors in `bootstrapCoreScript`). No CORS headers — ever. The
same-origin bounce page is the only supported caller; browsers block
everything else, which is intended.

### A.4 Bounce page: `GET /teams/sso?return=…` (HTTP.hs) + core reuse

Two pieces, replacing the previously planned separate Teams client core:

**(a) `bootstrapCoreScript` becomes frame-aware (decision 9).**
`BootstrapConfig` gains `teamsSsoPath :: !Text` (default `"/teams/sso"`);
`loginUrl()` branches on `window.self !== window.top` — framed →
`AUTH_BASE + teamsSsoPath + '?return=' + encodeURIComponent(location.href)`,
unframed → the existing `/auth/login` form. `retryUrl()` inherits the branch
for free. Everything else (fragment contract, `/api/login` exchange, error
dispatch, sessionStorage caching, redirect-vs-report rule) is untouched, and
every consumer of the core is thereby Teams-capable with zero app changes —
nothing ever navigates to AAD inside a frame. (Dev mode `authBaseUrl =
Nothing` keeps its onToken(null) path; the framed branch only matters when
an AUTH_BASE exists.)

**(b) New Servant route on marmay-auth** (NOT under `/auth/` — it must be
frameable, see A.5):

```
"teams" :> "sso" :> QueryParam "return" Text :> Get '[HTML] Html
```

Server-side, before rendering: `return` present + `parseAbsoluteURI` +
`isAllowedReturnUrl` (else a plain HTML error page — same checks as
`loginHandler`). The rendered page:

1. `<script>` tag for teams-js 2.x from the Microsoft CDN, pinned version
   (`res.cdn.office.net/teams-js/…`) — the auth service has no static-file
   machinery and this page is Microsoft-facing anyway; vendoring buys nothing
   here.
2. Inline script: `microsoftTeams.app.initialize()` →
   `authentication.getAuthToken()` (v2 promise form) →
   `POST /auth/teams/exchange?return=<passthrough>` (octet-stream, raw
   token) → `{assertion}` →
   `location.replace(RETURN + '#itoken=' + assertion)`. `RETURN` is injected
   server-side via `jsonText` (it is already validated).
3. Errors: no navigation — render an in-page German panel (mirror the
   competences shell panels): `{error: "unknown-user"}` → "kein Benutzer"
   text; anything else (incl. `initialize`/`getAuthToken` rejection, i.e.
   opened outside Teams) → failure text + the raw error detail (debuggability
   over polish; teachers see this page only when something is broken) + a
   reload link (`location.reload()` — Teams caches AAD tokens; retries are
   cheap).
4. `notifySuccess`/`notifyFailure` are NOT called and the manifest does not
   set `showLoadingIndicatorOnAppLoad`, so Teams never waits on them —
   keeps teams-js completely off the instance page.

### A.5 Response headers (app/Main.hs)

Wrap `serve` in a small middleware (add `wai-extra`, or hand-roll with
`mapResponseHeaders` — it's ~10 lines either way):

- `/auth/*`: `Cache-Control: no-store` and
  `Content-Security-Policy: frame-ancestors 'none'` (nothing under `/auth/`
  is ever legitimately framed — including the exchange endpoint; today there
  are no framing headers at all).
- `/teams/*` (`/teams/sso` now, `/teams/config` in Phase C):
  `frame-ancestors` = the Teams allowlist — make it config
  (`teamsFrameAncestors :: [Text]` with the default list
  `teams.microsoft.com *.teams.microsoft.com *.office.com *.microsoft365.com
  *.cloud.microsoft`; verify against current MS docs at implementation time).
  `/teams/sso` additionally gets `Cache-Control: no-store` (it is an auth
  page in all but path). Decided at implementation (2026-09-01): **no
  `script-src` on the sso page for the PoC** — a nonce would need
  per-request coordination between the middleware (header) and the handler
  (script tag), which is not "one line" with the path-keyed middleware
  design; the page's only dynamic datum is the server-validated return URI
  (no `<` possible, argued at `ssoScript`), so the injection surface a
  `script-src` would guard is already closed. Revisit post-PoC if the page
  ever grows more dynamic content.

### A.6 Tests (test/Main.hs)

Same style as the existing seven protocol tests; no network:

- JWKS document parsing: a captured real discovery JSON → `JWKSet` with >0 keys.
- Exchange validation: generate an RSA JWK in the test, self-sign tokens with
  the AAD claim shape, inject the public `JWKSet` into a `JWKSCache` directly
  (export a constructor for tests or make the TVar reachable): accept-path
  (`aud` = bare clientId GUID, per the gate finding), wrong `iss`, wrong
  `aud`, expired, `alg` ≠ RS256, missing `preferred_username`+`upn`+`email`,
  lowercasing of the identity.
- Return-URL validation table (shared with the browser flow's checks):
  subdomain, apex, evil-domain, `evil<apex>` (suffix trick without the dot),
  userinfo, port, http, fragment present.
- Audience-derivation parity: the exchange and `callbackHandler` produce the
  same `aud` for the same return URL (the shared-helper refactor in A.3
  step 6 makes this near-tautological — keep the test as a tripwire anyway).
- End-to-end pairing: exchange output validates with
  `validateIdentityAssertion'` against the matching origin and fails against a
  sibling origin (mirrors the existing wrong-audience test).

### A.7 Release + pin bump

Commit/push marmay-auth; in competences bump the `source-repository-package`
tag + `--sha256:` (`nix flake prefetch github:marmay/marmay-auth --json`).
Redeploy the auth service (nixosModule picks up the new package). No config
change needed — `teamsConfig` defaults when absent (A.1).

---

## Phase B — competences: CSP + pin bump (shrunk by decision 9)

Decision 9 removed the Teams shell variant, the `/teams/*` instance routes,
the path mapping, and the WASM change: the ordinary `/app` shell with the
frame-aware core covers Teams. Historical sub-sections: B.1 vendoring
dropped (decision 7 — the bounce page CDN-loads teams-js), B.2/B.3/B.5
dropped (decision 9), B.6 nginx proxy dropped (decision 7 — the exchange is
same-origin on the auth host; the auth service's own vhost is all the nginx
there is). What remains:

### B.4 CSP middleware: new `Competences.Backend.Middleware`

- `securityHeaders :: [Text] {- frame-ancestors allowlist -} -> Wai.Middleware`
  — hand-rolled: inspect `Wai.pathInfo`, `mapResponseHeaders` on the response:
  - path head `"app"` → `Content-Security-Policy: <base>; frame-ancestors
    <allowlist entries space-separated>` (Teams must be able to frame the
    app itself now; the allowlist is Microsoft hosts only, so arbitrary-site
    framing stays blocked).
  - everything else (`/`, `/login`, …) → `<base>; frame-ancestors 'none'`
    (closes the long-standing X-Frame-Options gap noted in Shell.hs).
    `/static/*` keeps no frame-ancestors header for now — inert for
    subresources, and it keeps the throwaway gate page frameable while
    Phase A testing still uses it.
- `<base>` = move `cspHeaderValue` here from Shell.hs; **delete the CSP meta
  tag** from `renderShell` (meta CSP can't carry frame-ancestors and
  double-delivery invites drift).
- Config: `SecurityConfig` (competences) gains
  `teamsFrameAncestors :: ![Text]` — optional with the default Microsoft list
  (extend to a manual `parseJSON` mirroring marmay-auth's `SecurityConfig`
  if it's currently generic).
- Wire in `app-backend/Main.hs`: wrap only the HTTP side —
  `websocketsOr defaultConnectionOptions (wsHandler …) (securityHeaders allow httpApp)`
  (WS upgrades bypass it; nothing to add there).

### B.7 Pin bump

Bump the marmay-auth pin so the shell serves the frame-aware core (A.4a) and
the oid-keyed assertion consumption (A.0c) — no WASM rebuild needed for
Teams (`marmay-auth` stays non-buildable on wasm32; the bootstrap arrives as
server-rendered JS).

---

## Phase C — Config page, manifest, pilot

**Framing (2026-09-01, decision 10): the Teams app is "BG Horn" — a
meta-app / application selector.** One catalog entry, one manifest, one AAD
registration; the config page selects WHICH internal application (and
instance) a tab shows. Any trust-domain app qualifies as a target once it
(a) allows Teams framing (a B.4-style frame-ancestors opt-in) and (b) runs
the frame-aware bootstrap core — the CMS lacks only (a). New application =
one registry entry + that app's frameability; no tenant-admin work.
Limitation to know: Teams app-permission policies gate the whole catalog
entry, not individual selector targets — per-application access control
remains each app's own login (unknown users get the "kein Benutzer" panel).

### C.1 `/teams/config` on marmay-auth

- The registry
  (`data ApplicationEntry = ApplicationEntry { name, contentUrl, websiteUrl :: !Text }`,
  full URLs per entry — entry points differ across apps) is **public
  config, not part of the encrypted SecurityConfig** (implemented
  2026-09-01): loaded from a separate unencrypted file via the new
  `--applications` flag (`loadPublicConfigFile` — the secrets loader
  refuses world-readable files, which nix-store paths are). The
  marmay-auth nixosModule renders `services.marmay-auth.applications`
  (typed list option) into that file; competences' and the CMS's modules
  publish read-only `teamsApplications` outputs derived from their
  instance attrsets, so a single-host config wires them with one
  concatenation (split topologies provide entries manually). Keeps "new
  class = one nix attr"; the agenix secret never changes for registry
  reasons.
- New route `"teams" :> "config" :> Get '[HTML] Html`: a `<select>` over
  `applications`; on selection `setValidityState(true)`; save handler calls
  `pages.config.setConfig` with `entityId` = entry name, `contentUrl` =
  `https://auth.<apex>/teams/sso?return=` ++ urlencoded entry `contentUrl`
  (the bounce, decisions 7+9 — the tab opens on the auth host, the app URL
  rides in `return`; cold load never depends on in-tab navigation),
  `websiteUrl` = the entry's `websiteUrl` (the free "open in browser"
  escape hatch, incl. mobile), `suggestedDisplayName` = entry name. The
  `applications` list doubles as the hardening option for A.3: exact
  allowlist instead of suffix-match — post-PoC.
- teams-js for this page: load from Microsoft's CDN
  (`res.cdn.office.net/teams-js/…`), same as `/teams/sso` (A.4) — the service
  has no static-file machinery, the page is teacher-only and
  Microsoft-facing; vendoring buys nothing. (Instances load no teams-js at
  all — decision 7.)
- Later "more views": a second dropdown (view per tab) that varies the path
  suffix — the deep-link mechanics from B.2 already support any route.

### C.2 Manifest package (`teams/` in **marmay-auth** — relocated
2026-09-01: the manifest describes the meta-app, and the meta-app is the
auth service; competences is just one selectable target)

- `manifest.json`, schema ≥ 1.17: **new random GUID** as the Teams app `id`
  (not the AAD clientId); name/description/icons branded **"BG Horn"** (the
  school's application portal — decision 10; per-tab display names come
  from `suggestedDisplayName` at configuration time); `configurableTabs:
  [{ configurationUrl: "https://auth.<apex>/teams/config", scopes:
  ["team"], canUpdateConfiguration: true }]`; `validDomains: ["<apex>", "*.<apex>"]`
  (wildcard excludes the apex — both entries needed; the auth subdomain is
  covered by the wildcard); `webApplicationInfo: { id: <clientId>, resource:
  "api://auth.<apex>/<clientId>" }` — the auth host, per the gate result:
  the resource authority must exactly match the origin that calls
  `getAuthToken`, which is only ever `/teams/sso`. **Do not** set
  `showLoadingIndicatorOnAppLoad` (A.4 step 4 / B.2 step 4 rely on it being
  off). The Entra Anwendungs-ID-URI moves from the Phase 0 test value
  (`api://m2a.<test-apex>/<clientId>`) to `api://auth.<apex>/<clientId>` at
  this point — admin consent survives; retarget the sideloaded gate app in
  the same step if it's still wanted as a token generator.
- Icons: `color.png` 192×192, `outline.png` 32×32 — derive from the inline
  SVG favicon in Shell.hs.
- `package.sh`: zip the three files.

### C.3 Pilot

Sideload/custom-upload into ONE class Team; run the verification matrix below;
let it sit for a while before org-catalog + app-permission-policy (only
teachers may add tabs) — that step is post-PoC.

---

## Verification matrix

Exchange endpoint (curl, gate-page token):
- real token + allowed `return` → assertion that `validateIdentityAssertion'`
  accepts for that instance origin and rejects for a sibling
- tampered signature / wrong `aud` / expired → 401 `invalid-token`
- missing/unparseable `return` → 400 `invalid-return`; off-domain `return` →
  403 `disallowed-return`
- assertion replay: redeem twice at `/api/login` → second is 403 (existing
  jti protection, now covering the Teams path)

Headers (`curl -I`): instance `/app/...` → frame-ancestors allowlist (decision
9); `/`, other shell paths → `frame-ancestors 'none'`; auth service
`/auth/...` → `no-store` + `'none'`; `/teams/sso` → allowlist + `no-store`;
`/teams/config` → allowlist. Confirm the meta CSP tag is gone and the header
carries the full former value.

**Early check (do this FIRST, right after the bounce page exists, before
building B):** in-tab navigation → SSO. Sideload a tab whose `contentUrl` is
an instance `/app` page (which, framed and token-less, redirects itself to
`/teams/sso`) — then confirm `initialize()` + `getAuthToken()` still succeed
silently *after* navigation. The cold-load path (contentUrl = bounce
directly, per C.1) is the gate-proven pattern; the re-auth path (24 h
expiry, retry links, pasted `/app` contentUrls) is exactly this navigated
variant and Teams' rules here are the same family that failed us once
already. If it fails: expiry inside Teams degrades to an error panel saying
"close and reopen the tab", and pasted `/app` contentUrls don't work —
decide then whether that's acceptable.

In Teams (sideloaded tab): add/configure/rename/remove lifecycle; tab loads
silently (no consent popup) for teacher and student accounts; deep link lands
on the assignments page with the newest Hausübung selected; WebSocket works in
the iframe; non-member of the class opening the tab → clean "kein Benutzer"
panel (not a redirect, not a spinner); browser `/app/…` flow still works
end-to-end (regression); a plain `/app/…` URL configured as a tab works
directly (frame-aware core); short `tokenExpiryDuration`/short session JWT in a test
config → silent re-auth on reload inside Teams, no loop. Second class in a
second Team once the first is green.

Load: cold-load the tab in Teams desktop + web on a school-typical machine
(the 11 MB WASM question — Stage 4 decides on mobile, but note desktop numbers
now).

---

## Risks / long-term notes (beyond teams-integration-plan.md's list)

- **`return`-param trust in the exchange** is bounded by `isAllowedReturnUrl`
  (A.3 step 2) — keep that check when refactoring; it is the only thing tying
  assertion audiences to the trust domain on this path. (Same trust model,
  same code path, as the browser flow's return URL.)
- **In-tab navigation → getAuthToken** (expiry/retry path) is the one
  Teams-rules bet the gate did not cover — verified early per the
  verification matrix; cold load does not depend on it.
- **`preferred_username` vs `mail`** (0.2) — dissolved by decision 8: the
  identity key is `oid` (immutable), UPN is only the provisioning matcher.
  Residual: consumers must implement lazy oid-binding correctly (match oid
  first, upn second, backfill).
- **Full app in the tab** means Teams students see the complete nav, not a
  kiosk. If a locked-down Teams mode is ever wanted, key it off framed-ness
  (`window.self !== window.top`, the same signal the core uses) — cheap, but
  a product decision, not plumbing.
- **Microsoft churn** stays confined to config/docs: authorized-client GUID
  list (Azure), `teamsFrameAncestors` (config), manifest schema, CDN URL of
  the config page's teams-js.
- The exchange endpoint has **no rate limiting**; it does full RS256
  validation on arbitrary input. Fine at school scale behind nginx; note for
  any future public deployment.
