-- | Stateless disclosure (expand/collapse) rendering helpers.
--
-- Provides a chevron indicator and collapsible card components
-- for bordered, expandable sections with clickable headers.
--
-- == Usage
--
-- @
-- -- Simple disclosure with text title
-- Disclosure.disclosure toggleAction $
--   Disclosure.contents (Disclosure.titleText "Section Title") isExpanded sectionContent []
--
-- -- With actions
-- Disclosure.disclosure toggleAction $
--   Disclosure.contents (Disclosure.titleText "Title") isExpanded bodyView
--     [ Action Icon.IcnEdit editAction
--     , DestructiveAction Icon.IcnDelete deleteAction
--     ]
--
-- -- With icon and title
-- Disclosure.disclosure toggleAction $
--   Disclosure.contents (Disclosure.titleIconText Icon.IcnTask "Task 1.2.3") isExpanded bodyView []
--
-- -- With left/right content (e.g., title + badge)
-- Disclosure.disclosure toggleAction $
--   Disclosure.contents (Disclosure.titleWithAnnotation leftView rightView) isExpanded bodyView []
--
-- -- Nested style (inside another disclosure)
-- Disclosure.innerDisclosure toggleAction $
--   Disclosure.contents (Disclosure.titleText "Nested Item") isExpanded nestedContent []
--
-- -- Custom palette
-- Disclosure.paletteDisclosure myPalette toggleAction $
--   Disclosure.contents (Disclosure.titleText "Styled") isExpanded styledContent []
-- @
module Competences.Frontend.View.Disclosure
  ( -- * Core types
    DisclosureStyle (..)
  , DisclosureAction (..)
  , DisclosureContents (..)

    -- * Content construction
  , contents

    -- * Title helpers
  , titleIcon
  , titleIconText
  , titleText
  , titleWithAnnotation

    -- * Presentation functions
  , disclosure
  , innerDisclosure
  , paletteDisclosure
  , innerPaletteDisclosure
  , maybePaletteDisclosure

    -- * Low-level helpers
  , disclosureChevron
  )
where

import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Color (PaletteColor (..), PaletteName, bgClass)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Tailwind (class_)
import Data.Text (Text)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.String (MisoString)

-- ============================================================================
-- Core Types
-- ============================================================================

-- | Style variants for disclosures.
data DisclosureStyle
  = -- | Bordered card with full padding (default)
    DisclosureDefault
  | -- | Borderless, reduced padding for nesting
    DisclosureNested
  deriving (Eq, Show)

-- | Action buttons in the disclosure header.
data DisclosureAction a
  = -- | Ghost button (subtle, for secondary actions)
    Action !Icon.Icon !a
  | -- | Destructive button (red, for delete actions)
    DestructiveAction !Icon.Icon !a
  deriving (Eq, Show)

-- | Disclosure configuration with content.
-- The title is a full View, allowing arbitrary content (icons, badges, etc.)
data DisclosureContents m a = DisclosureContents
  { title :: !(M.View m a)
  , body :: !(Maybe (M.View m a))
  , actions :: ![DisclosureAction a]
  }

-- ============================================================================
-- Content Construction
-- ============================================================================

-- | Create disclosure contents.
--
-- The body is evaluated lazily: when @expanded@ is @False@, the @bodyView@
-- thunk is not forced, so expensive views are not computed when collapsed.
--
-- @
-- contents (titleText "Title") isExpanded bodyView [Action Icon.IcnEdit editAction]
-- contents (titleIconText Icon.IcnTask "Task") isExpanded bodyView []
-- @
contents
  :: M.View m a
  -- ^ Title view (use titleText, titleIconText, or titleWithAnnotation)
  -> Bool
  -- ^ Whether the disclosure is expanded
  -> M.View m a
  -- ^ Body content (lazily evaluated)
  -> [DisclosureAction a]
  -- ^ Header actions
  -> DisclosureContents m a
contents titleView expanded bodyView actions' =
  DisclosureContents
    { title = titleView
    , body = if expanded then Just bodyView else Nothing
    , actions = actions'
    }

-- ============================================================================
-- Title Helpers
-- ============================================================================

-- | Title with just an icon.
titleIcon :: Icon.Icon -> M.View m a
titleIcon icon = Icon.icon [] icon

-- | Title with icon and text (common case).
titleIconText :: Icon.Icon -> MisoString -> M.View m a
titleIconText icon text =
  MH.div_
    [class_ "flex items-center gap-2 min-w-0"]
    [ Icon.icon [] icon
    , MH.span_ [class_ "font-medium truncate"] [M.text text]
    ]

-- | Title with just text.
titleText :: MisoString -> M.View m a
titleText t = MH.span_ [class_ "font-medium truncate"] [M.text t]

-- | Title with left and right content.
-- The right content appears on the far right side of the header.
-- Useful for adding badges, status indicators, or other annotations.
titleWithAnnotation :: M.View m a -> M.View m a -> M.View m a
titleWithAnnotation left right =
  MH.div_
    [class_ "flex items-center justify-between w-full"]
    [ MH.div_ [class_ "min-w-0 flex-1"] [left]
    , MH.div_ [class_ "shrink-0 ml-2"] [right]
    ]

-- ============================================================================
-- Presentation Functions
-- ============================================================================

-- | Disclosure with default style and muted background.
--
-- This is the primary disclosure function for most use cases.
--
-- @
-- disclosure toggleAction $
--   contents "Section" isExpanded sectionContent []
-- @
disclosure :: a -> DisclosureContents m a -> M.View m a
disclosure = disclosureImpl DisclosureDefault Nothing

-- | Disclosure with nested style (no border, reduced padding).
--
-- Use inside another disclosure for hierarchical content.
--
-- @
-- innerDisclosure toggleAction $
--   contents "Nested Item" isExpanded nestedContent []
-- @
innerDisclosure :: a -> DisclosureContents m a -> M.View m a
innerDisclosure = disclosureImpl DisclosureNested Nothing

-- | Disclosure with default style and custom palette.
--
-- @
-- paletteDisclosure statusPalette toggleAction $
--   contents "Styled Section" isExpanded styledContent []
-- @
paletteDisclosure :: PaletteName -> a -> DisclosureContents m a -> M.View m a
paletteDisclosure p = disclosureImpl DisclosureDefault (Just p)

-- | Disclosure with nested style and custom palette.
--
-- @
-- innerPaletteDisclosure statusPalette toggleAction $
--   contents (titleText "Nested Styled") isExpanded nestedContent []
-- @
innerPaletteDisclosure :: PaletteName -> a -> DisclosureContents m a -> M.View m a
innerPaletteDisclosure p = disclosureImpl DisclosureNested (Just p)

-- | Disclosure with optional palette.
-- When @Nothing@, uses the default muted background.
-- When @Just palette@, uses the palette's base color.
--
-- @
-- maybePaletteDisclosure mStatus toggleAction $
--   contents (titleText "Item") isExpanded content []
-- @
maybePaletteDisclosure :: Maybe PaletteName -> a -> DisclosureContents m a -> M.View m a
maybePaletteDisclosure Nothing = disclosure
maybePaletteDisclosure (Just p) = paletteDisclosure p

-- ============================================================================
-- Implementation
-- ============================================================================

-- | Core implementation for all disclosure variants.
disclosureImpl
  :: DisclosureStyle
  -> Maybe PaletteName
  -> a
  -> DisclosureContents m a
  -> M.View m a
disclosureImpl style mPalette toggleAction dc =
  MH.div_
    [class_ containerClasses]
    [ MH.div_
        [ class_ headerClasses
        , MH.onClick toggleAction
        ]
        headerContent
    , bodySection
    ]
  where
    isExpanded = case dc.body of
      Just _ -> True
      Nothing -> False

    -- Container classes based on style
    containerClasses = case style of
      DisclosureDefault -> "border rounded-lg overflow-hidden" :: Text
      DisclosureNested -> "rounded overflow-hidden"

    -- Header background class
    headerBg = case mPalette of
      Nothing -> "bg-muted/50" :: Text
      Just p -> bgClass Base p

    -- Header classes based on style
    headerClasses = case style of
      DisclosureDefault ->
        "flex items-center gap-3 px-3 py-2 cursor-pointer hover:bg-muted transition-colors " <> headerBg
      DisclosureNested ->
        "flex items-center gap-2 px-2 py-1.5 cursor-pointer hover:bg-muted/50 transition-colors " <> headerBg

    -- Header content
    headerContent =
      [ disclosureChevron isExpanded
      , MH.div_ [class_ "flex-1 min-w-0"] [dc.title]
      ]
        <> actionsView

    actionsView = case dc.actions of
      [] -> []
      as -> [MH.div_ [class_ "flex gap-1 shrink-0"] (map renderAction as)]

    -- Body section
    bodySection = case dc.body of
      Nothing -> M.text ""
      Just bodyView ->
        MH.div_ [class_ bodyClasses] [bodyView]

    bodyClasses = case style of
      DisclosureDefault -> "px-3 py-2 border-t" :: Text
      DisclosureNested -> "pl-6 pr-2 py-1.5 border-t border-muted"

-- | Render an action button.
-- Note: We need to use an explicit type application or specify the action type
-- to avoid overlapping instances with Button.ToAction.
renderAction :: forall m a. DisclosureAction a -> M.View m a
renderAction (Action icon act) =
  Button.ghostSm (Button.ButtonConfig (Button.IconOnly icon) (Just act))
renderAction (DestructiveAction icon act) =
  Button.destructiveSm (Button.ButtonConfig (Button.IconOnly icon) (Just act))

-- ============================================================================
-- Low-level Helpers
-- ============================================================================

-- | Chevron indicator for expand/collapse.
-- Shows a right-pointing arrow when collapsed, down-pointing when expanded.
disclosureChevron :: Bool -> M.View m a
disclosureChevron isExpanded =
  Icon.icon [] (if isExpanded then Icon.IcnArrowDown else Icon.IcnExpandShrinkArrowRight)
