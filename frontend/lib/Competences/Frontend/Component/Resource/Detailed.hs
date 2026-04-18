-- | Detailed resource view: state machine, view primitives, effectful update,
-- and full Miso component.
module Competences.Frontend.Component.Resource.Detailed
  ( -- * State machine
    ResourceDetailedState (..)
  , ResourceDetailedAction (..)
  , initialResourceDetailedState
  , updateResourceDetailedPure
    -- * Embeddable update
  , updateResourceDetailed
    -- * Resource rendering
  , renderResource
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
    -- * Full component
  , ResourceDetailedConfig (..)
  , ResourceDetailedSettings (..)
  , defaultResourceDetailedSettings
  , resourceDetailedComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Common.Set (toggle)
import Competences.Document (Document (..), FileRef (..), Resource (..), ResourceContent (..), ResourceId, ResourceIdentifier (..), User)
import Competences.Frontend.Component.EntityMenu qualified as EM
import Competences.Frontend.Component.FileGallery (fileGalleryComponent)
import Competences.Frontend.Component.RichContent (renderRichTextWithFiles)
import Competences.Frontend.Page (Page (..))
import Competences.Frontend.SyncContext
  ( PinViewerRequest (..)
  , ProjectedChange (..)
  , SyncContext (..)
  , isTeacher
  , subscribeWithProjection
  )
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
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
import Optics.Core (Lens', (%~), (.~))

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
-- Embeddable update
-- ============================================================================

-- | Embeddable update: pass a lens at the parent's 'ResourceDetailedState'.
updateResourceDetailed
  :: Lens' model ResourceDetailedState
  -> ResourceDetailedAction
  -> M.Effect parent model action
updateResourceDetailed stateLens action =
  M.modify (stateLens %~ updateResourceDetailedPure action)

-- ============================================================================
-- Resource rendering
-- ============================================================================

renderResource
  :: SyncContext
  -> ResourceDetailedState
  -> (Resource -> [M.View m a])
  -> (ResourceDetailedAction -> a)
  -> Resource
  -> M.View m a
renderResource r state mkAnnotations liftAction res =
  let ResourceIdentifier identText = res.identifier
      displayName = if T.null identText then "(Unbenannt)" else identText
      expanded = Set.member res.id state.expandedResources
      toggleRes = liftAction (ToggleResource res.id)
      annotations = mkAnnotations res
   in case res.content of
        InlineContent rc
          | rc == mempty ->
              resourceStaticHeader
                (resourceContentIcon res.content)
                (ms displayName)
                annotations
          | otherwise ->
              resourceDisclosureView
                (resourceContentIcon res.content)
                toggleRes
                (ms displayName)
                annotations
                expanded
                (resourceContentView (renderRichTextWithFiles r.formulaCache r res.attachments rc))
        WebLink url title ->
          linkCardView
            (resourceContentIcon res.content)
            identText
            displayName
            url
            title
            annotations
        VideoLink url title ->
          linkCardView
            (resourceContentIcon res.content)
            identText
            displayName
            url
            title
            annotations
        FileContent fileRef ->
          resourceDisclosureView
            (resourceContentIcon res.content)
            toggleRes
            (ms displayName)
            annotations
            expanded
            (inlineComponent
                ("res-gallery-" <> ms (show fileRef.hash))
                (fileGalleryComponent r [fileRef]))

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

-- ============================================================================
-- Full component
-- ============================================================================

data ResourceDetailedConfig = ResourceDetailedConfig
  { resourceId :: !ResourceId
  , settings :: !ResourceDetailedSettings
  }

data ResourceDetailedSettings = ResourceDetailedSettings
  { startExpanded :: !Bool
  , showAnnotations :: !Bool
  , enableGoTo :: !Bool
  , enableDelete :: !Bool
  }
  deriving (Eq, Show)

defaultResourceDetailedSettings :: ResourceDetailedSettings
defaultResourceDetailedSettings =
  ResourceDetailedSettings
    { startExpanded = True
    , showAnnotations = True
    , enableGoTo = True
    , enableDelete = False
    }

newtype ResourceProjection = ResourceProjection
  { resource :: Maybe Resource
  }
  deriving (Eq, Generic, Show)

data ComponentModel = ComponentModel
  { projection :: !ResourceProjection
  , viewState :: !ResourceDetailedState
  }
  deriving (Eq, Generic, Show)

data ComponentAction
  = ProjectionChanged !(ProjectedChange ResourceProjection)
  | ViewAction !ResourceDetailedAction
  deriving (Eq, Show)

resourceDetailedComponent :: SyncContext -> ResourceDetailedConfig -> M.Component p ComponentModel ComponentAction
resourceDetailedComponent r cfg =
  (M.component model update' view')
    { M.subs = [subscribeWithProjection r (resourceProjection cfg) ProjectionChanged]
    }
  where
    model =
      ComponentModel
        { projection = ResourceProjection {resource = Nothing}
        , viewState =
            initialResourceDetailedState
              [cfg.resourceId | cfg.settings.startExpanded]
        }

    update' (ProjectionChanged change) = M.modify $ #projection .~ change.projection
    update' (ViewAction a) = updateResourceDetailed #viewState a

    view' m = case m.projection.resource of
      Nothing -> Layout.empty
      Just res -> renderResource r m.viewState (annotations m) ViewAction res

    annotations _m res
      | cfg.settings.showAnnotations, isTeacher r =
          [ inlineComponent ("entity-menu-" <> ms (show res.id))
              (EM.entityMenuComponent r EM.EntityMenuConfig
                { edit = Just (EM.resourceEdit res.id)
                , pin = Just (PinResourceViewer res)
                , goTo = if cfg.settings.enableGoTo then Just (ManageResources (Just res.id)) else Nothing
                , delete = if cfg.settings.enableDelete then Just (EM.resourceDelete res.id) else Nothing
                , extraEntries = []
                })
          ]
      | otherwise = []

resourceProjection :: ResourceDetailedConfig -> Document -> Maybe User -> ResourceProjection
resourceProjection cfg doc _mUser =
  ResourceProjection {resource = Ix.getOne (doc.resources Ix.@= cfg.resourceId)}
