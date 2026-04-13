-- | This module provides definitions of icons used in the application.
-- Usage is pretty simple, include `iconDefs` in your view to enable
-- the usage of icons. Then use `icon` to insert an icon in your view.
--
-- While I would like to provide icons via a separate file, I did not
-- manage to get it to work for now.
module Competences.Frontend.View.Icon
  ( Icon (..)
  , Variant (..)
  , Size (..)
  , Animation (..)
  , iconDefs
  , icon
  , iconV
  , iconS
  , iconVS
  , iconFull
  , variantStrokeClass
  , sizeClass
  , animationClass
  )
where

import Data.Text (Text)
import Miso (Attribute, View)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as M
import Miso.String (MisoString, ms)
import Miso.Svg qualified as MS
import Miso.Svg.Property qualified as MSP

data Icon
  = IcnView
  | IcnEdit
  | IcnDelete
  | IcnAdd
  | IcnApply
  | IcnCancel
  | IcnArrowUp
  | IcnArrowDown
  | IcnArrowLeft
  | IcnArrowRight
  | IcnDoubleArrowUp
  | IcnDoubleArrowDown
  | IcnExpandShrinkArrowLeft
  | IcnExpandShrinkArrowRight
  | IcnReorder
  | IcnActivityTypeConversation
  | IcnActivityTypeExam
  | IcnActivityTypeSchoolExercise
  | IcnActivityTypeHomeExercise
  | IcnSocialFormIndividual
  | IcnSocialFormGroup
  | IcnTask

  | IcnCompetenceGrid
  | IcnEvidence
  | IcnAssignment
  | IcnInfo
  | IcnLock
  | IcnLockOpen
  | IcnProgress
  | IcnAbilitySelfReliant
  | IcnAbilitySillyMistakes
  | IcnAbilityWithSupport
  | IcnAbilityNotYet
  | IcnSolution
  | IcnResources
  | IcnLink
  | IcnVideo
  | IcnImport
  | IcnExport
  | IcnMesoPlan
  | IcnPin
  | IcnWarning
  | IcnPlus
  | IcnPlusPlus
  | IcnMinus
  | IcnMinusMinus
  | IcnPrint
  | IcnMenu
  | IcnLessonNotes
  | IcnOpenModal -- ^ Square with arrow out (open in modal)
  | IcnCloudCheck -- ^ Connected, all synced (cloud + checkmark)
  | IcnCloudSync -- ^ Connected, sending/receiving (cloud + arrows)
  | IcnCloudOff -- ^ Disconnected (cloud + slash)
  | IcnSick -- ^ Thermometer (absence/sick)
  | IcnStar -- ^ Star (assessment marker)
  deriving (Bounded, Eq, Enum, Ord, Show)

-- | Icon color variants based on theme colors
data Variant
  = Primary       -- ^ --primary color (for emphasis)
  | Secondary     -- ^ --secondary-foreground (subtle)
  | Ghost         -- ^ --muted-foreground (very subtle)
  | Destructive   -- ^ --destructive color (danger/delete actions)
  | OnPrimary     -- ^ --primary-foreground (for icons inside primary containers)
  deriving (Bounded, Eq, Enum, Ord, Show)

-- | Icon size variants to match button sizes
data Size
  = Small    -- ^ 16px (w-4 h-4) - for small buttons
  | Regular  -- ^ 20px (w-5 h-5) - for regular buttons
  | Large    -- ^ 24px (w-6 h-6) - for large buttons (current default)
  | XLarge   -- ^ 28px (w-7 h-7) - for nav bar icons
  deriving (Bounded, Eq, Enum, Ord, Show)

-- | Animation variants for icons
data Animation
  = Static -- ^ No animation
  | Pulse -- ^ animate-pulse (gentle pulsing opacity)
  deriving (Bounded, Eq, Enum, Ord, Show)

-- | CSS class for animation
animationClass :: Animation -> Text
animationClass = \case
  Static -> ""
  Pulse -> "animate-pulse"

-- | CSS class for variant stroke color
variantStrokeClass :: Variant -> Text
variantStrokeClass = \case
  Primary -> "text-primary"
  Secondary -> "text-secondary-foreground"
  Ghost -> "text-muted-foreground"
  Destructive -> "text-destructive"
  OnPrimary -> "text-primary-foreground"

-- | CSS classes for icon size
sizeClass :: Size -> Text
sizeClass = \case
  Small -> "w-4 h-4"
  Regular -> "w-5 h-5"
  Large -> "w-6 h-6"
  XLarge -> "w-7 h-7"

-- | Render icon with variant-based coloring
iconV :: Variant -> Icon -> View m a
iconV variant icn =
  MH.span_
    [M.class_ $ ms $ variantStrokeClass variant]
    [icon [] icn]

-- | Render icon with specific size
iconS :: Size -> Icon -> View m a
iconS size icn =
  MS.svg_
    [ MSP.viewBox_ "0 0 24 24"
    , M.class_ $ ms $ sizeClass size
    , MSP.fill_ "none"
    , MSP.stroke_ "currentColor"
    ]
    [MS.use_ [M.href_ $ "#" <> iconId icn]]

-- | Render icon with variant and size
iconVS :: Variant -> Size -> Icon -> View m a
iconVS variant size icn =
  MH.span_
    [M.class_ $ ms $ variantStrokeClass variant]
    [iconS size icn]

-- | Render icon with variant, size, and animation
iconFull :: Variant -> Size -> Animation -> Icon -> View m a
iconFull variant size anim icn =
  MH.span_
    [M.class_ $ ms $ variantStrokeClass variant <> " " <> animationClass anim]
    [iconS size icn]

iconDefs :: View m a
iconDefs = MS.svg_ [M.width_ "0", M.height_ "0"] [MS.defs_ [] (map iconDefOf [minBound .. maxBound])]

icon :: [Attribute a] -> Icon -> View m a
icon attrs icn =
  MS.svg_
    ( [ MSP.viewBox_ "0 0 24 24"
      , M.width_ "24"
      , M.height_ "24"
      , MSP.fill_ "none"           -- No fill by default
      , MSP.stroke_ "currentColor" -- Inherit text color for stroke
      ]
      <> attrs  -- Allow overriding defaults
    )
    [MS.use_ [M.href_ $ "#" <> iconId icn]]

iconId :: Icon -> MisoString
iconId = \case
  IcnView -> "icon-eye"
  IcnEdit -> "icon-pen"
  IcnDelete -> "icon-trash"
  IcnAdd -> "icon-plus"
  IcnApply -> "icon-check"
  IcnCancel -> "icon-x"
  IcnArrowUp -> "icon-arrow-up"
  IcnArrowDown -> "icon-arrow-down"
  IcnArrowLeft -> "icon-arrow-left"
  IcnArrowRight -> "icon-arrow-right"
  IcnDoubleArrowUp -> "icon-arrow-up-double"
  IcnDoubleArrowDown -> "icon-arrow-down-double"
  IcnExpandShrinkArrowLeft -> "icon-expand-shrink-arrow-left"
  IcnExpandShrinkArrowRight -> "icon-expand-shrink-arrow-right"
  IcnReorder -> "icon-reorder"
  IcnActivityTypeConversation -> "icon-activity-type-conversation"
  IcnActivityTypeExam -> "icon-activity-type-supervised"
  IcnActivityTypeSchoolExercise -> "icon-activity-type-semi-supervised"
  IcnActivityTypeHomeExercise -> "icon-activity-type-unsupervised"
  IcnSocialFormIndividual -> "icon-social-form-individual"
  IcnSocialFormGroup -> "icon-social-form-group"
  IcnTask -> "icon-task"

  IcnCompetenceGrid -> "icon-competence-grid"
  IcnEvidence -> "icon-evidence"
  IcnAssignment -> "icon-assignment"
  IcnInfo -> "icon-info"
  IcnLock -> "icon-lock"
  IcnLockOpen -> "icon-lock-open"
  IcnProgress -> "icon-progress"
  IcnAbilitySelfReliant -> "icon-ability-self-reliant"
  IcnAbilitySillyMistakes -> "icon-ability-silly-mistakes"
  IcnAbilityWithSupport -> "icon-ability-with-support"
  IcnAbilityNotYet -> "icon-ability-not-yet"
  IcnSolution -> "icon-solution"
  IcnResources -> "icon-resources"
  IcnLink -> "icon-link"
  IcnVideo -> "icon-video"
  IcnImport -> "icon-import"
  IcnExport -> "icon-export"
  IcnMesoPlan -> "icon-meso-plan"
  IcnPin -> "icon-pin"
  IcnWarning -> "icon-warning"
  IcnPlus -> "icon-plus-simple"
  IcnPlusPlus -> "icon-plus-plus"
  IcnMinus -> "icon-minus"
  IcnMinusMinus -> "icon-minus-minus"
  IcnPrint -> "icon-print"
  IcnMenu -> "icon-menu"
  IcnLessonNotes -> "icon-lesson-notes"
  IcnOpenModal -> "icon-open-modal"
  IcnCloudCheck -> "icon-cloud-check"
  IcnCloudSync -> "icon-cloud-sync"
  IcnCloudOff -> "icon-cloud-off"
  IcnSick -> "icon-sick"
  IcnStar -> "icon-star"

iconDefOf :: Icon -> View m a
iconDefOf icn = MS.symbol_ [M.id_ $ iconId icn, MSP.viewBox_ "0 0 24 24"] (iconDefOf' icn)

iconDefOf' :: Icon -> [View m a]
iconDefOf' = \case
  IcnView ->
    -- Eye icon (view/preview)
    [ MS.path_
        [ MSP.d_ "M12 9C10.3431 9 9 10.3431 9 12C9 13.6569 10.3431 15 12 15C13.6569 15 15 13.6569 15 12C15 10.3431 13.6569 9 12 9Z"
        , MSP.strokeWidth_ "1.5"
        ]
    , MS.path_
        [ MSP.d_ "M2.45825 12C3.73253 7.94288 7.52281 5 12.0004 5C16.4781 5 20.2684 7.94291 21.5426 12C20.2684 16.0571 16.4781 19 12.0005 19C7.52281 19 3.73251 16.0571 2.45825 12Z"
        , MSP.strokeWidth_ "1.5"
        ]
    ]
  IcnEdit ->
    [ MS.path_
        [ MSP.d_
            "M14.3601 4.07866L15.2869 3.15178C16.8226 1.61607 19.3125 1.61607 20.8482 3.15178C22.3839 4.68748 22.3839 7.17735 20.8482 8.71306L19.9213 9.63993M14.3601 4.07866C14.3601 4.07866 14.4759 6.04828 16.2138 7.78618C17.9517 9.52407 19.9213 9.63993 19.9213 9.63993M14.3601 4.07866L5.83882 12.5999C5.26166 13.1771 4.97308 13.4656 4.7249 13.7838C4.43213 14.1592 4.18114 14.5653 3.97634 14.995C3.80273 15.3593 3.67368 15.7465 3.41556 16.5208L2.32181 19.8021M19.9213 9.63993L11.4001 18.1612C10.8229 18.7383 10.5344 19.0269 10.2162 19.2751C9.84082 19.5679 9.43469 19.8189 9.00498 20.0237C8.6407 20.1973 8.25352 20.3263 7.47918 20.5844L4.19792 21.6782M4.19792 21.6782L3.39584 21.9456C3.01478 22.0726 2.59466 21.9734 2.31063 21.6894C2.0266 21.4053 1.92743 20.9852 2.05445 20.6042L2.32181 19.8021M4.19792 21.6782L2.32181 19.8021"
        , MSP.strokeWidth_ "1.5"
        ]
    ]
  IcnDelete ->
    [ MS.path_
        [ MSP.d_ "M20.5001 6H3.5"
        , MSP.strokeWidth_ "1.5"
        , MSP.strokeLinecap_ "round"
        ]
    , MS.path_
        [ MSP.d_
            "M18.8332 8.5L18.3732 15.3991C18.1962 18.054 18.1077 19.3815 17.2427 20.1907C16.3777 21 15.0473 21 12.3865 21H11.6132C8.95235 21 7.62195 21 6.75694 20.1907C5.89194 19.3815 5.80344 18.054 5.62644 15.3991L5.1665 8.5"
        , MSP.strokeWidth_ "1.5"
        , MSP.strokeLinecap_ "round"
        ]
    , MS.path_
        [ MSP.d_ "M9.5 11L10 16"
        , MSP.strokeWidth_ "1.5"
        , MSP.strokeLinecap_ "round"
        ]
    , MS.path_
        [ MSP.d_ "M14.5 11L14 16"
        , MSP.strokeWidth_ "1.5"
        , MSP.strokeLinecap_ "round"
        ]
    , MS.path_
        [ MSP.d_
            "M6.5 6C6.55588 6 6.58382 6 6.60915 5.99936C7.43259 5.97849 8.15902 5.45491 8.43922 4.68032C8.44784 4.65649 8.45667 4.62999 8.47434 4.57697L8.57143 4.28571C8.65431 4.03708 8.69575 3.91276 8.75071 3.8072C8.97001 3.38607 9.37574 3.09364 9.84461 3.01877C9.96213 3 10.0932 3 10.3553 3H13.6447C13.9068 3 14.0379 3 14.1554 3.01877C14.6243 3.09364 15.03 3.38607 15.2493 3.8072C15.3043 3.91276 15.3457 4.03708 15.4286 4.28571L15.5257 4.57697C15.5433 4.62992 15.5522 4.65651 15.5608 4.68032C15.841 5.45491 16.5674 5.97849 17.3909 5.99936C17.4162 6 17.4441 6 17.5 6"
        , MSP.strokeWidth_ "1.5"
        ]
    ]
  IcnAdd ->
    [ MS.circle_
        [ MSP.cx_ "12"
        , MSP.cy_ "12"
        , MSP.r_ "10"
        , MSP.strokeWidth_ "1.5"
        ]
    , MS.path_
        [ MSP.d_ "M15 12L12 12M12 12L9 12M12 12L12 9M12 12L12 15"
        , MSP.strokeWidth_ "1.5"
        , MSP.strokeLinecap_ "round"
        ]
    ]
  IcnApply ->
    [ MS.circle_
        [ MSP.cx_ "12"
        , MSP.cy_ "12"
        , MSP.r_ "10"
        , MSP.strokeWidth_ "1.5"
        ]
    , MS.path_
        [ MSP.d_ "M8.5 12.5L10.5 14.5L15.5 9.5"
        , MSP.strokeWidth_ "1.5"
        , MSP.strokeLinecap_ "round"
        , MSP.strokeLinejoin_ "round"
        ]
    ]
  IcnCancel ->
    [ MS.circle_
        [ MSP.cx_ "12"
        , MSP.cy_ "12"
        , MSP.r_ "10"
        , MSP.strokeWidth_ "1.5"
        ]
    , MS.path_
        [ MSP.d_ "M14.5 9.50002L9.5 14.5M9.49998 9.5L14.5 14.5"
        , MSP.strokeWidth_ "1.5"
        , MSP.strokeLinecap_ "round"
        ]
    ]
  IcnArrowUp ->
    mkPathesDR
      [ "M19 15L12 9L5 15"
      ]
  IcnArrowDown ->
    mkPathesDR
      [ "M19 9L12 15L5 9"
      ]
  IcnArrowLeft ->
    mkPathesDR
      [ "M15 5L9 12L15 19"
      ]
  IcnArrowRight ->
    mkPathesDR
      [ "M9 5L15 12L9 19"
      ]
  IcnDoubleArrowUp ->
    mkPathesDR
      [ "M19 13L12 7L5 13"
      , "M19 17L12 11L5 17"
      ]
  IcnDoubleArrowDown ->
    mkPathesDR
      [ "M19 11L12 17L5 11"
      , "M19 7L12 13L5 7"
      ]
  -- todo: this is ai generated, look for proper icon!
  IcnExpandShrinkArrowLeft ->
    mkPathesDR
      [ "M15 18L9 12L15 6"
      ]
  -- todo: this is ai generated, look for proper icon!
  IcnExpandShrinkArrowRight ->
    mkPathesDR
      [ "M9 18L15 12L9 6"
      ]
  IcnReorder ->
    mkPathesDR
      [ "M16 18L16 6M16 6L20 10.125M16 6L12 10.125"
      , "M8 6L8 18M8 18L12 13.875M8 18L4 13.875"
      ]
  IcnActivityTypeConversation ->
    mkPathesD
      [ "M8 9h8"
      , "M8 13h6"
      , "M18 4a3 3 0 0 1 3 3v8a3 3 0 0 1 -3 3h-5l-5 3v-3h-2a3 3 0 0 1 -3 -3v-8a3 3 0 0 1 3 -3h12z"
      ]
  IcnActivityTypeExam ->
    mkPathesD
      [ "M5 3m0 2a2 2 0 0 1 2 -2h10a2 2 0 0 1 2 2v14a2 2 0 0 1 -2 2h-10a2 2 0 0 1 -2 -2z"
      , "M9 7l6 0"
      , "M9 11l6 0"
      , "M9 15l4 0"
      ]
  IcnActivityTypeSchoolExercise ->
    mkPathesD
      [ "M6 11.4999H7M6 15.4999H7M17 15.4999H18M17 11.4999H18M11.5 11.4999H12.5M10 20.9999V16.9999C10 15.8954 10.8954 14.9999 12 14.9999C13.1046 14.9999 14 15.8954 14 16.9999V20.9999M17 7.49995L18.5761 7.89398C19.4428 8.11064 19.8761 8.21898 20.1988 8.46057C20.4834 8.67373 20.7061 8.95895 20.8439 9.28682C21 9.65843 21 10.1051 21 10.9984V17.7999C21 18.9201 21 19.4801 20.782 19.9079C20.5903 20.2843 20.2843 20.5902 19.908 20.782C19.4802 20.9999 18.9201 20.9999 17.8 20.9999H6.2C5.0799 20.9999 4.51984 20.9999 4.09202 20.782C3.71569 20.5902 3.40973 20.2843 3.21799 19.9079C3 19.4801 3 18.9201 3 17.7999V10.9984C3 10.1051 3 9.65843 3.15613 9.28682C3.29388 8.95895 3.51657 8.67373 3.80124 8.46057C4.12389 8.21898 4.55722 8.11064 5.42388 7.89398L7 7.49995L9.85931 4.92657C10.6159 4.2456 10.9943 3.90512 11.4221 3.77598C11.799 3.66224 12.201 3.66224 12.5779 3.77598C13.0057 3.90512 13.3841 4.2456 14.1407 4.92657L17 7.49995Z"
      ]
  IcnActivityTypeHomeExercise ->
    mkPathesD
      [ "M8 15.0001C8 15.0001 9.6 17.0001 12 17.0001C14.4 17.0001 16 15.0001 16 15.0001M3 14.6001V12.1302C3 10.9815 3 10.4071 3.14805 9.87819C3.2792 9.40966 3.49473 8.96898 3.78405 8.5778C4.11067 8.1362 4.56404 7.78358 5.47078 7.07834L8.07078 5.05612C9.47608 3.96311 10.1787 3.4166 10.9546 3.20653C11.6392 3.02116 12.3608 3.02116 13.0454 3.20653C13.8213 3.4166 14.5239 3.96311 15.9292 5.05612L18.5292 7.07834C19.436 7.78358 19.8893 8.1362 20.2159 8.5778C20.5053 8.96898 20.7208 9.40966 20.8519 9.87819C21 10.4071 21 10.9815 21 12.1302V14.6001C21 16.8403 21 17.9604 20.564 18.816C20.1805 19.5687 19.5686 20.1806 18.816 20.5641C17.9603 21.0001 16.8402 21.0001 14.6 21.0001H9.4C7.15979 21.0001 6.03969 21.0001 5.18404 20.5641C4.43139 20.1806 3.81947 19.5687 3.43597 18.816C3 17.9604 3 16.8403 3 14.6001Z"
      ]
  IcnSocialFormIndividual ->
    mkPathesD
      [ "M8 7a4 4 0 1 0 8 0a4 4 0 0 0 -8 0"
      , "M6 21v-2a4 4 0 0 1 4 -4h4a4 4 0 0 1 4 4v2"
      ]
  IcnSocialFormGroup ->
    mkPathesD
      [ "M10 13a2 2 0 1 0 4 0a2 2 0 0 0 -4 0"
      , "M8 21v-1a2 2 0 0 1 2 -2h4a2 2 0 0 1 2 2v1"
      , "M15 5a2 2 0 1 0 4 0a2 2 0 0 0 -4 0"
      , "M17 10h2a2 2 0 0 1 2 2v1"
      , "M5 5a2 2 0 1 0 4 0a2 2 0 0 0 -4 0"
      , "M3 13v-1a2 2 0 0 1 2 -2h2"
      ]
  -- Document/file icon for tasks
  IcnTask ->
    mkPathesD
      [ "M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"
      , "M14 2v6h6"
      , "M16 13H8"
      , "M16 17H8"
      , "M10 9H8"
      ]

  -- Grid icon for competence grids
  IcnCompetenceGrid ->
    mkPathesD
      [ "M3 3h7v7H3z"
      , "M14 3h7v7h-7z"
      , "M3 14h7v7H3z"
      , "M14 14h7v7h-7z"
      ]
  -- Clipboard/check icon for evidences
  IcnEvidence ->
    mkPathesD
      [ "M9 11l3 3L22 4"
      , "M21 12v7a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h11"
      ]
  -- Clipboard icon for assignments
  IcnAssignment ->
    mkPathesD
      [ "M16 4h2a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h2"
      , "M15 2H9a1 1 0 0 0-1 1v2a1 1 0 0 0 1 1h6a1 1 0 0 0 1-1V3a1 1 0 0 0-1-1z"
      ]
  -- Info icon (circle with i)
  IcnInfo ->
    mkPathesDR
      [ "M12 22c5.523 0 10-4.477 10-10S17.523 2 12 2 2 6.477 2 12s4.477 10 10 10z"
      , "M12 16v-4"
      , "M12 8h.01"
      ]
  -- Lock icon (closed)
  IcnLock ->
    mkPathesDR
      [ "M5 13a2 2 0 0 1 2-2h10a2 2 0 0 1 2 2v6a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2v-6z"
      , "M8 11V7a4 4 0 1 1 8 0v4"
      ]
  -- Lock open icon
  IcnLockOpen ->
    mkPathesDR
      [ "M5 13a2 2 0 0 1 2-2h10a2 2 0 0 1 2 2v6a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2v-6z"
      , "M8 11V7a4 4 0 1 1 8 0"
      ]
  -- Progress icon: zig-zag line going up (growth/learning)
  IcnProgress ->
    mkPathesDR
      [ "M3 18l4 -4l3 3l4 -5l4 4l3 -6"  -- zig-zag line trending upward
      ]
  -- Ability icons (for observation display)
  -- Double checkmark: SelfReliant (fully mastered)
  IcnAbilitySelfReliant ->
    mkPathesDR
      [ "M4 12l4 4l8 -8"     -- First checkmark
      , "M8 16l4 4l8 -8"     -- Second checkmark (offset down-right)
      ]
  -- Single checkmark: SelfReliantWithSillyMistakes (mastered with minor errors)
  IcnAbilitySillyMistakes ->
    mkPathesDR
      [ "M5 12l5 5l9 -9"
      ]
  -- Half circle: WithSupport (partial mastery, needs help)
  IcnAbilityWithSupport ->
    [ MS.path_
        [ MSP.d_ "M12 2C6.48 2 2 6.48 2 12s4.48 10 10 10"
        , MSP.strokeWidth_ "1.5"
        , MSP.fill_ "none"
        , MSP.strokeLinecap_ "round"
        ]
    ]
  -- X mark: NotYet (not mastered)
  IcnAbilityNotYet ->
    mkPathesDR
      [ "M18 6L6 18"
      , "M6 6l12 12"
      ]
  -- Lightbulb icon for solutions (hints, answers)
  IcnSolution ->
    mkPathesD
      [ "M9 21h6"
      , "M12 3a6 6 0 0 0-6 6c0 1.8 .8 3.4 2 4.5V15a1 1 0 0 0 1 1h6a1 1 0 0 0 1-1v-1.5c1.2-1.1 2-2.7 2-4.5a6 6 0 0 0-6-6z"
      , "M9 18h6"
      ]
  -- Book/resources icon
  IcnResources ->
    mkPathesD
      [ "M4 19.5A2.5 2.5 0 0 1 6.5 17H20"
      , "M6.5 2H20v20H6.5A2.5 2.5 0 0 1 4 19.5v-15A2.5 2.5 0 0 1 6.5 2z"
      ]
  -- Link/chain icon
  IcnLink ->
    mkPathesDR
      [ "M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71"
      , "M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71"
      ]
  -- Video/play icon
  IcnVideo ->
    mkPathesDR
      [ "M23 7l-7 5 7 5V7z"
      , "M14 5H3a2 2 0 0 0-2 2v10a2 2 0 0 0 2 2h11a2 2 0 0 0 2-2V7a2 2 0 0 0-2-2z"
      ]
  -- Import icon (arrow pointing into box)
  IcnImport ->
    mkPathesDR
      [ "M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"
      , "M7 10l5 5 5-5"
      , "M12 15V3"
      ]
  -- Export icon (arrow pointing out of box)
  IcnExport ->
    mkPathesDR
      [ "M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"
      , "M17 8l-5-5-5 5"
      , "M12 3v12"
      ]
  -- Meso plan icon (calendar with list - planning schedule)
  IcnMesoPlan ->
    mkPathesD
      [ "M19 4H5a2 2 0 0 0-2 2v14a2 2 0 0 0 2 2h14a2 2 0 0 0 2-2V6a2 2 0 0 0-2-2z"
      , "M16 2v4"
      , "M8 2v4"
      , "M3 10h18"
      , "M8 14h8"
      , "M8 18h5"
      ]
  -- Thumbtack/pin icon
  IcnPin ->
    mkPathesDR
      [ "M12 17v5"
      , "M5 17h14"
      , "M18 8l-1.5 9h-9L6 8"
      , "M15 3H9a1 1 0 0 0-1 1v4h8V4a1 1 0 0 0-1-1z"
      ]
  -- Warning triangle with exclamation mark
  IcnWarning ->
    mkPathesDR
      [ "M12 9v4"
      , "M12 17h.01"
      , "M10.29 3.86L1.82 18a2 2 0 0 0 1.71 3h16.94a2 2 0 0 0 1.71-3L13.71 3.86a2 2 0 0 0-3.42 0z"
      ]
  -- Simple plus sign (no circle)
  IcnPlus ->
    [ MS.path_
        [ MSP.d_ "M12 5V19M5 12H19"
        , MSP.strokeWidth_ "2"
        , MSP.strokeLinecap_ "round"
        ]
    ]
  -- Double plus (two plus signs side by side)
  IcnPlusPlus ->
    [ MS.path_
        [ MSP.d_ "M7 7V17M3 12H11M17 7V17M13 12H21"
        , MSP.strokeWidth_ "2"
        , MSP.strokeLinecap_ "round"
        ]
    ]
  -- Simple minus sign (smaller)
  IcnMinus ->
    [ MS.path_
        [ MSP.d_ "M7 12H17"
        , MSP.strokeWidth_ "2"
        , MSP.strokeLinecap_ "round"
        ]
    ]
  -- Double minus (two minus signs side by side with gap)
  IcnMinusMinus ->
    [ MS.path_
        [ MSP.d_ "M3 12H10M14 12H21"
        , MSP.strokeWidth_ "2"
        , MSP.strokeLinecap_ "round"
        ]
    ]
  -- Printer icon (Lucide printer)
  IcnPrint ->
    mkPathesDR
      [ "M6 18H4a2 2 0 0 1-2-2v-5a2 2 0 0 1 2-2h16a2 2 0 0 1 2 2v5a2 2 0 0 1-2 2h-2"
      , "M6 9V3h12v6"
      , "M6 15h12v6H6z"
      ]
  -- Hamburger menu icon (3 horizontal lines)
  IcnMenu ->
    mkPathesDR
      [ "M4 6h16"
      , "M4 12h16"
      , "M4 18h16"
      ]
  -- Lesson notes icon (notebook with lines)
  IcnLessonNotes ->
    mkPathesD
      [ "M4 4a2 2 0 0 1 2-2h12a2 2 0 0 1 2 2v16a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V4z"
      , "M8 2v20"
      , "M11 7h5"
      , "M11 11h5"
      , "M11 15h3"
      ]
  -- External link / open-in-modal icon (square with arrow pointing out)
  IcnOpenModal ->
    mkPathesDR
      [ "M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6"
      , "M15 3h6v6"
      , "M10 14L21 3"
      ]
  -- Cloud + checkmark in bottom-right (connected, synced)
  IcnCloudCheck ->
    mkPathesDR
      [ cloudBase
      , "M13 21l2 2 4-4" -- checkmark
      ]
  -- Cloud + up/down arrows in bottom-right (connected, syncing)
  IcnCloudSync ->
    mkPathesDR
      [ cloudBase
      , "M14 22v-5l-2 2m2-2l2 2" -- up arrow
      , "M19 17v5l-2-2m2 2l2-2" -- down arrow
      ]
  -- Cloud + diagonal slash (disconnected)
  IcnCloudOff ->
    mkPathesDR
      [ cloudBase
      , "M2 2l20 20" -- slash
      ]
  -- Thermometer icon (sick/absent)
  IcnSick ->
    mkPathesDR
      [ "M14 14.76V3.5a2.5 2.5 0 0 0-5 0v11.26a4.5 4.5 0 1 0 5 0z"
      ]
  -- Star icon (Lucide star, filled polygon)
  IcnStar ->
    [ MS.path_
        [ MSP.d_ "M12 2l3.09 6.26L22 9.27l-5 4.87 1.18 6.88L12 17.77l-6.18 3.25L7 14.14 2 9.27l6.91-1.01L12 2z"
        , MSP.strokeWidth_ "1.5"
        , MSP.fill_ "currentColor"
        , MSP.strokeLinecap_ "round"
        , MSP.strokeLinejoin_ "round"
        ]
    ]
  where
    -- Consistent cloud outline shared by all cloud status icons (Lucide cloud)
    cloudBase :: M.MisoString
    cloudBase = "M4 14.899A7 7 0 1 1 15.71 8h1.79a4.5 4.5 0 0 1 2.5 8.242"
    mkPathes :: [M.Attribute a] -> [M.MisoString] -> [M.View m a]
    mkPathes as = map (\p -> MS.path_ (MSP.d_ p : as))
    mkPathesD = mkPathes [MSP.strokeWidth_ "1.5", MSP.fill_ "none"]
    mkPathesDR =
      mkPathes
        [MSP.strokeWidth_ "1.5", MSP.fill_ "none", MSP.strokeLinecap_ "round", MSP.strokeLinejoin_ "round"]
