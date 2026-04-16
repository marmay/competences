-- | Detailed resource view: pure view primitives and pure state machine.
--
-- Effects for the state machine live in 'Component.Resource.Detailed.Embed'.
module Competences.Frontend.Fragment.Resource.Detailed
  ( -- * State machine
    ResourceDetailedState (..)
  , ResourceDetailedAction (..)
  , initialResourceDetailedState
  , updateResourceDetailedPure
    -- * Icons
  , resourceContentIcon
    -- * Header
  , resourceHeader
  , resourceHeaderWithBadges
    -- * Content
  , resourceContentView
    -- * Composites
  , resourceDisclosureView
  , resourceStaticHeader
  , linkCardView
  )
where

import Competences.Common.Set (toggle)
import Competences.Document (ResourceContent (..), ResourceId)
import Competences.Frontend.View.Disclosure qualified as Disclosure
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Miso.String (MisoString, ms)
import Optics.Core ((%~))

-- ============================================================================
-- State machine
-- ============================================================================

newtype ResourceDetailedState = ResourceDetailedState
  { expandedResources :: Set ResourceId
  }
  deriving (Eq, Generic, Show)

newtype ResourceDetailedAction
  = ToggleResource ResourceId
  deriving (Eq, Show)

initialResourceDetailedState :: [ResourceId] -> ResourceDetailedState
initialResourceDetailedState expanded =
  ResourceDetailedState {expandedResources = Set.fromList expanded}

updateResourceDetailedPure :: ResourceDetailedAction -> ResourceDetailedState -> ResourceDetailedState
updateResourceDetailedPure (ToggleResource rid) = #expandedResources %~ toggle rid


-- ============================================================================
-- Icons
-- ============================================================================

-- | Pick an icon for a resource content variant.
resourceContentIcon :: ResourceContent -> Icon.Icon
resourceContentIcon = \case
  WebLink _ _ -> Icon.IcnLink
  VideoLink _ _ -> Icon.IcnVideo
  _ -> Icon.IcnResources

-- ============================================================================
-- Header
-- ============================================================================

resourceHeader :: Icon.Icon -> MisoString -> M.View m a
resourceHeader = Disclosure.titleIconText

resourceHeaderWithBadges :: Icon.Icon -> MisoString -> [M.View m a] -> M.View m a
resourceHeaderWithBadges icn displayName extras =
  Disclosure.titleWithAnnotation
    (Disclosure.titleIconText icn displayName)
    (Layout.hFlow (Layout.gapS <> Layout.hFull <> Layout.crossCenter) extras)

-- ============================================================================
-- Content
-- ============================================================================

-- | Wrap pre-rendered rich content in a prose container.
resourceContentView :: M.View m a -> M.View m a
resourceContentView renderedContent =
  MH.div_
    [class_ "prose prose-stone prose-sm max-w-none"]
    [renderedContent]

-- ============================================================================
-- Composites
-- ============================================================================

-- | Collapsible resource view (disclosure). For inline resources with a body.
resourceDisclosureView
  :: Icon.Icon
  -> a
  -- ^ Toggle action
  -> MisoString
  -- ^ Display name
  -> [M.View m a]
  -- ^ Header annotations (right side)
  -> Bool
  -- ^ Expanded
  -> M.View m a
  -- ^ Body content
  -> M.View m a
resourceDisclosureView icn toggleAction displayName annotations isExpanded body =
  Disclosure.innerDisclosure toggleAction $
    Disclosure.contents (resourceHeaderWithBadges icn displayName annotations) isExpanded body []

-- | Non-expandable resource header (no body content).
resourceStaticHeader
  :: Icon.Icon
  -> MisoString
  -- ^ Display name
  -> [M.View m a]
  -- ^ Annotations (right side)
  -> M.View m a
resourceStaticHeader icn displayName annotations =
  MH.div_
    [class_ "border rounded-lg overflow-hidden"]
    [ MH.div_
        [class_ "flex items-center justify-between px-3 py-2"]
        [ resourceHeader icn displayName
        , Layout.hFlow (Layout.gapS <> Layout.crossCenter) annotations
        ]
    ]

-- | Render a link-style resource (web or video) with optional annotations.
--
-- The link covers the left portion of the row; annotations (e.g. an edit
-- button) sit at the right without competing for the click.
linkCardView
  :: Icon.Icon
  -> Text
  -- ^ Identifier (shown when it differs from the link title)
  -> Text
  -- ^ Display name
  -> Text
  -- ^ URL
  -> Text
  -- ^ Link title / description
  -> [M.View m a]
  -- ^ Annotations (right side)
  -> M.View m a
linkCardView icon ident displayName url title annotations =
  MH.div_
    [class_ "flex items-center gap-2 px-4 py-3 rounded-lg hover:bg-muted/50 transition-colors"]
    [ MH.a_
        [ class_ "flex-1 flex items-center gap-2"
        , MP.href_ (ms url)
        , MP.target_ "_blank"
        , MP.rel_ "noopener noreferrer"
        ]
        [ Icon.icon [class_ "text-sky-600"] icon
        , MH.span_ [class_ "font-medium"] [M.text (ms displayName)]
        , if T.null title || title == ident
            then Layout.empty
            else MH.span_ [class_ "text-muted-foreground text-sm truncate"] [M.text (ms $ "— " <> title)]
        ]
    , Layout.hFlow (Layout.gapS <> Layout.crossCenter) annotations
    ]
