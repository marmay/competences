module Competences.Frontend.Component.AboutDialog
  ( aboutButtonView
  )
where

import Competences.Frontend.BuildInfo (frontendVersion)
import Competences.Frontend.SyncContext
  ( SyncContext (..)
  , readServerInfo
  )
import Competences.Frontend.SyncContext.WindowManager
  ( WindowManagerRef
  , closeModal
  , openModal
  )
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Component (component)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Modal qualified as Modal
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Tooltip (Tooltip (..), withTooltip)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Protocol (ServerInfo (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP

-- ============================================================================
-- INFO BUTTON (in nav bar)
-- ============================================================================

-- | Info button view for the nav bar, matching the connection status style.
aboutButtonView :: SyncContext -> M.View p a
aboutButtonView ir = component "about-button" (aboutButtonComponent ir)

data ButtonModel = ButtonModel
  deriving (Eq, Generic, Show)

data ButtonAction = OpenAboutDialog
  deriving (Eq, Show)

aboutButtonComponent :: SyncContext -> M.Component p ButtonModel ButtonAction
aboutButtonComponent ir = M.component model update view
  where
    model = ButtonModel

    update OpenAboutDialog = M.io_ $ do
      srvInfo <- readServerInfo ir
      openModal ir.windowManager (aboutModalComponent ir.windowManager srvInfo)

    view _m =
      withTooltip (PlainTooltip "Info") $
        Button.primaryLg (Button.button Icon.IcnInfo OpenAboutDialog)

-- ============================================================================
-- ABOUT MODAL
-- ============================================================================

newtype ModalModel = ModalModel
  { serverInfo :: ServerInfo
  }
  deriving (Eq, Generic, Show)

data ModalAction = CloseAbout
  deriving (Eq, Show)

aboutModalComponent :: WindowManagerRef -> ServerInfo -> M.Component p ModalModel ModalAction
aboutModalComponent wmRef srvInfo = M.component model update view
  where
    model = ModalModel srvInfo

    update CloseAbout = M.io_ $ closeModal wmRef

    view m =
      Modal.modalDialog
        []
        [ Modal.modalHeader "Meine Kompetenzen" CloseAbout
        , modalBody m
        , Modal.modalFooter
            [ Button.secondary (Button.button ("Schlie\223en" :: M.MisoString) CloseAbout)
            ]
        ]

modalBody :: ModalModel -> M.View m ModalAction
modalBody m =
  MH.div_
    [class_ "px-6 py-4"]
    [ Layout.vFlow
        Layout.gapM
        [ versionSection m
        , licenseSection
        , copyrightSection
        ]
    ]

versionSection :: ModalModel -> M.View m a
versionSection m =
  Layout.vFlow
    Layout.gapS
    [ Typography.h4 "Version"
    , versionRow "Frontend" frontendVersion
    , versionRow "Backend" m.serverInfo.backendVersion
    ]

versionRow :: M.MisoString -> Text -> M.View m a
versionRow label ver =
  Layout.hFlow
    (Layout.gapS <> Layout.crossCenter)
    [ Typography.small label
    , Typography.code (M.ms ver)
    ]

licenseSection :: M.View m a
licenseSection =
  Layout.vFlow
    Layout.gapS
    [ Typography.h4 "Lizenz"
    , Typography.paragraph "BSD-3-Clause"
    , MH.a_
        [ class_ "text-sm text-sky-600 hover:underline"
        , MP.href_ "https://github.com/marmay/competences"
        , MP.target_ "_blank"
        ]
        [M.text "GitHub: marmay/competences"]
    ]

copyrightSection :: M.View m a
copyrightSection =
  Typography.muted "\169 2025\8211\&2026 Markus Mayr"
