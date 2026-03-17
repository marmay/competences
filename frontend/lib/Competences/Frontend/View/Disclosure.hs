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
--     [ Disclosure.action Icon.IcnEdit editAction
--     , Disclosure.destructiveAction Icon.IcnDelete deleteAction
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
  , DisclosureAction
  , DisclosureContents (..)

    -- * Action constructors
  , action
  , destructiveAction
  , holdDestructiveAction

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
  , popDisclosure
  , innerPopDisclosure
  , paletteDisclosure
  , innerPaletteDisclosure
  , maybePaletteDisclosure

    -- * Low-level helpers
  , disclosureChevron
  )
where

import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Color (PaletteColor (..), PaletteName, bgClass)
import Competences.Frontend.View.HoldButton qualified as HoldButton
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
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

-- | Action buttons in the disclosure header (opaque – use smart constructors).
newtype DisclosureAction m a = DisclosureAction (M.View m a)

-- | Disclosure configuration with content.
-- The title is a full View, allowing arbitrary content (icons, badges, etc.)
data DisclosureContents m a = DisclosureContents
  { title :: !(M.View m a)
  , body :: !(Maybe (M.View m a))
  , actions :: ![DisclosureAction m a]
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
  -> [DisclosureAction m a]
  -- ^ Header actions
  -> DisclosureContents m a
contents titleView expanded bodyView actions' =
  DisclosureContents
    { title = titleView
    , body = if expanded then Just bodyView else Nothing
    , actions = actions'
    }

-- ============================================================================
-- Action Constructors
-- ============================================================================

-- | Ghost button action (subtle, for secondary actions).
action :: Icon.Icon -> a -> DisclosureAction m a
action icon act = DisclosureAction $ Button.ghostSm (Button.ButtonConfig (Button.IconOnly icon) (Just act))

-- | Destructive button action (red, for delete actions).
destructiveAction :: Icon.Icon -> a -> DisclosureAction m a
destructiveAction icon act = DisclosureAction $ Button.destructiveSm (Button.ButtonConfig (Button.IconOnly icon) (Just act))

-- | Hold-to-delete action (press and hold 2s to confirm).
holdDestructiveAction :: (Eq id) => (HoldButton.HoldAction id -> a) -> HoldButton.HoldState id -> id -> DisclosureAction m a
holdDestructiveAction wrap hs eid = DisclosureAction $ HoldButton.holdDeleteButtonSm wrap hs eid

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
    [class_ "min-w-0"]
    [ Layout.hFlow
        (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
        [ Icon.icon [] icon
        , MH.span_ [class_ "font-medium truncate"] [M.text text]
        ]
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
    [class_ "w-full"]
    [ Layout.hFlow
        (Layout.hFull <> Layout.crossCenter <> Layout.mainBetween)
        [ MH.div_ [class_ "min-w-0 flex-1"] [left]
        , MH.div_ [class_ "shrink-0 ml-2"] [right]
        ]
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
disclosure = disclosureImpl DisclosureDefault MutedHeader

-- | Disclosure with nested style (no border, reduced padding).
--
-- Use inside another disclosure for hierarchical content.
--
-- @
-- innerDisclosure toggleAction $
--   contents "Nested Item" isExpanded nestedContent []
-- @
innerDisclosure :: a -> DisclosureContents m a -> M.View m a
innerDisclosure = disclosureImpl DisclosureNested MutedHeader

-- | Disclosure with default style and theme-primary accent.
--
-- Use to make a disclosure visually prominent (\"pop\") using the
-- theme's primary color, independent of any domain palette.
popDisclosure :: a -> DisclosureContents m a -> M.View m a
popDisclosure = disclosureImpl DisclosureDefault PopHeader

-- | Disclosure with nested style and theme-primary accent.
innerPopDisclosure :: a -> DisclosureContents m a -> M.View m a
innerPopDisclosure = disclosureImpl DisclosureNested PopHeader

-- | Disclosure with default style and custom palette.
--
-- @
-- paletteDisclosure statusPalette toggleAction $
--   contents "Styled Section" isExpanded styledContent []
-- @
paletteDisclosure :: PaletteName -> a -> DisclosureContents m a -> M.View m a
paletteDisclosure p = disclosureImpl DisclosureDefault (PaletteHeader p)

-- | Disclosure with nested style and custom palette.
--
-- @
-- innerPaletteDisclosure statusPalette toggleAction $
--   contents (titleText "Nested Styled") isExpanded nestedContent []
-- @
innerPaletteDisclosure :: PaletteName -> a -> DisclosureContents m a -> M.View m a
innerPaletteDisclosure p = disclosureImpl DisclosureNested (PaletteHeader p)

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

-- | Header color scheme.
data HeaderColor
  = MutedHeader
  | PopHeader
  | PaletteHeader !PaletteName

-- | Core implementation for all disclosure variants.
disclosureImpl
  :: DisclosureStyle
  -> HeaderColor
  -> a
  -> DisclosureContents m a
  -> M.View m a
disclosureImpl style headerColor toggleAction dc =
  MH.div_
    [class_ containerClasses]
    [ MH.div_
        [class_ headerWrapperExtra]
        [ Layout.addClass headerLayoutExtra $
            Layout.hFlow
              (Layout.hFull <> Layout.crossCenter)
              headerContent
        ]
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

    -- Header background + text color classes
    headerBg = case headerColor of
      MutedHeader -> "bg-muted/50" :: Text
      PopHeader -> "bg-primary text-primary-foreground"
      PaletteHeader p -> bgClass Base p

    -- Layout classes added via addClass
    headerLayoutExtra = case style of
      DisclosureDefault -> "gap-3" :: Text
      DisclosureNested -> "gap-2"

    -- Hover background class
    headerHover = case headerColor of
      MutedHeader -> case style of
        DisclosureDefault -> "hover:bg-muted" :: Text
        DisclosureNested -> "hover:bg-muted/50"
      PopHeader -> "hover:bg-primary/80"
      PaletteHeader _ -> case style of
        DisclosureDefault -> "hover:bg-muted"
        DisclosureNested -> "hover:bg-muted/50"

    -- Non-layout classes that go on wrapper div
    headerWrapperExtra = case style of
      DisclosureDefault ->
        "px-3 py-2 transition-colors " <> headerBg <> " " <> headerHover
      DisclosureNested ->
        "px-2 py-1.5 transition-colors " <> headerBg <> " " <> headerHover

    -- Header content
    headerContent =
      [ MH.div_
          [ class_ $ "flex items-center flex-1 min-w-0 cursor-pointer " <> headerLayoutExtra
          , MH.onClick toggleAction
          ]
          [ disclosureChevron isExpanded
          , MH.div_ [class_ "flex-1 min-w-0"] [dc.title]
          ]
      ]
        <> actionsView

    actionsView = case dc.actions of
      [] -> []
      as -> [MH.div_ [class_ "shrink-0"] [Layout.hFlow Layout.gapT (map renderAction as)]]

    -- Body section
    bodySection = case dc.body of
      Nothing -> M.text ""
      Just bodyView ->
        MH.div_ [class_ bodyClasses] [bodyView]

    bodyClasses = case style of
      DisclosureDefault -> "px-3 py-2 border-t" :: Text
      DisclosureNested -> "pl-6 pr-2 py-1.5 border-t border-muted"

-- | Render an action button.
renderAction :: DisclosureAction m a -> M.View m a
renderAction (DisclosureAction v) = v

-- ============================================================================
-- Low-level Helpers
-- ============================================================================

-- | Chevron indicator for expand/collapse.
-- Shows a right-pointing arrow when collapsed, down-pointing when expanded.
disclosureChevron :: Bool -> M.View m a
disclosureChevron isExpanded =
  Icon.icon [] (if isExpanded then Icon.IcnArrowDown else Icon.IcnExpandShrinkArrowRight)
