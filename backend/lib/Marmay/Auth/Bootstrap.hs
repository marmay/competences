{-# LANGUAGE RecordWildCards #-}

module Marmay.Auth.Bootstrap
  ( BootstrapConfig(..)
  , defaultBootstrapConfig
  , bootstrapCoreScript
  , jsonText
  )
  where

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE

-- | Parameters for generating the runAuthBootstrap function.
--
-- The default bootstrapping function assumes that you provide an end point
-- that consumes the IdentityAssertion of the auth service and mints its own
-- JWT. Bootstrapping will decode that token and re-use it.
data BootstrapConfig = BootstrapConfig
  { authBaseUrl :: !(Maybe Text)
    -- ^ Location of the auth service that mints the IdentityAssertion.
  , loginPath :: !Text
    -- ^ Login path of your application that consumes the IdentityAssertion
    -- and mints the session token.
  , storageKey :: !Text
    -- ^ Name that is used to store the session token.
  } deriving (Eq, Show)

defaultBootstrapConfig :: BootstrapConfig
defaultBootstrapConfig = BootstrapConfig
  { authBaseUrl = Nothing
  , loginPath = "/api/login"
  , storageKey = "sessionKey"
  }

-- | Render a value as a JS literal via its JSON encoding (string
-- escaping keeps values from breaking out of the literal; Maybe
-- becomes null).
jsonText :: Aeson.ToJSON a => a -> Text
jsonText = TL.toStrict . TLE.decodeUtf8 . Aeson.encode

-- | The client half of the assertion protocol: defines
-- @runAuthBootstrap(hooks)@ (definition only; the consuming app
-- appends the call). Expected hooks:
--
-- > runAuthBootstrap({
-- >   onToken:     function (jwt)      { ... start the app ... },
-- >   onNoAccount: function ()         { ... "no account" panel ... },
-- >   onFailure:   function (retryUrl) { ... failure panel ... }
-- > });
--
-- The core owns the fragment contract (@#itoken=@, scrubbed before
-- any await), the login exchange, error-code dispatch
-- ("unknown-user" -> onNoAccount, else onFailure), sessionStorage
-- caching, the client-side expiry check (60 s margin so a token
-- never dies mid-WebSocket-handshake -- the reason session tokens
-- must stay client-readable), and the redirect-vs-report rule: after
-- arriving with a fragment we never redirect (we just came from the
-- auth service; redirecting would loop), without a usable token we
-- always do. @onToken@ receives @null@ in dev mode (no
-- 'authBaseUrl'); @onFailure@'s @retryUrl@ is @null@ in dev mode.
bootstrapCoreScript :: BootstrapConfig -> Text
bootstrapCoreScript BootstrapConfig{..} = T.unlines
  [ "function runAuthBootstrap(hooks) {"
  , "  var AUTH_BASE = " <> jsonText authBaseUrl <> ";"
  , "  var LOGIN_PATH = " <> jsonText loginPath <> ";"
  , "  var KEY = " <> jsonText storageKey <> ";"
  , ""
  , "  function loginUrl() {"
  , "    return AUTH_BASE + '/auth/login?return=' + encodeURIComponent(location.href);"
  , "  }"
  , ""
  , "  function retryUrl() {"
  , "    return AUTH_BASE ? loginUrl() : null;"
  , "  }"
  , ""
  , "  function isUsable(jwt) {"
  , "    try {"
  , "      var payload = JSON.parse(atob(jwt.split('.')[1].replace(/-/g, '+').replace(/_/g, '/')));"
  , "      return typeof payload.exp === 'number' && payload.exp > Date.now() / 1000 + 60;"
  , "    } catch (e) {"
  , "      return false;"
  , "    }"
  , "  }"
  , ""
  , "  var match = location.hash.match(/^#itoken=(.+)$/);"
  , "  if (match) {"
  , "    var assertion = match[1];"
  , "    history.replaceState(null, '', location.pathname + location.search);"
  , "    fetch(LOGIN_PATH, {"
  , "      method: 'POST',"
  , "      headers: { 'Content-Type': 'application/octet-stream' },"
  , "      body: assertion"
  , "    }).then(function (resp) {"
  , "      if (resp.ok) {"
  , "        return resp.json().then(function (data) {"
  , "          sessionStorage.setItem(KEY, data.jwt);"
  , "          hooks.onToken(data.jwt);"
  , "        });"
  , "      }"
  , "      return resp.json().catch(function () { return {}; }).then(function (data) {"
  , "        if (data.error === 'unknown-user') {"
  , "          hooks.onNoAccount();"
  , "        } else {"
  , "          hooks.onFailure(retryUrl());"
  , "        }"
  , "      });"
  , "    }).catch(function () {"
  , "      hooks.onFailure(retryUrl());"
  , "    });"
  , "    return;"
  , "  }"
  , ""
  , "  var stored = sessionStorage.getItem(KEY);"
  , "  if (stored && isUsable(stored)) {"
  , "    hooks.onToken(stored);"
  , "    return;"
  , "  }"
  , "  sessionStorage.removeItem(KEY);"
  , "  if (AUTH_BASE) {"
  , "    location.href = loginUrl();"
  , "  } else {"
  , "    hooks.onToken(null);"
  , "  }"
  , "}"
  ]

