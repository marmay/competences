-- | This module provides definitions of icons used in the application.
-- Usage is pretty simple, include `iconDefs` in your view to enable
-- the usage of icons. Then use `icon` to insert an icon in your view.
--
-- While I would like to provide icons via a separate file, I did not
-- manage to get it to work for now.
module Competences.Frontend.View.Icon
  ( Icon (..)
  , iconDefs
  , icon
  )
where

import Miso (Attribute, View)
import Miso.String (MisoString)
import qualified Miso.Html.Property as M
import qualified Miso.Svg.Property as MSP
import qualified Miso.Svg as MS

data Icon
  = IcnEdit
  | IcnDelete
  | IcnAdd
  | IcnApply
  | IcnCancel
  | IcnArrowUp
  | IcnArrowDown
  | IcnDoubleArrowUp
  | IcnDoubleArrowDown
  | IcnExpandShrinkArrowLeft
  | IcnExpandShrinkArrowRight
  | IcnReorder
  deriving (Bounded, Eq, Enum, Ord, Show)

iconDefs :: View m a
iconDefs = MS.svg_ [MSP.width_ "0", MSP.height_ "0"] [MS.defs_ [] (map iconDefOf [minBound .. maxBound])]

icon :: [Attribute a] -> Icon -> View m a
icon attrs icn =
  MS.svg_
    (attrs <> [MSP.viewBox_ "0 0 24 24", MSP.width_ "24", MSP.height_ "24"])
    [MS.use_ [M.href_ $ "#" <> iconId icn]]

iconId :: Icon -> MisoString
iconId = \case
  IcnEdit -> "icon-pen"
  IcnDelete -> "icon-trash"
  IcnAdd -> "icon-plus"
  IcnApply -> "icon-check"
  IcnCancel -> "icon-x"
  IcnArrowUp -> "icon-arrow-up"
  IcnArrowDown -> "icon-arrow-down"
  IcnDoubleArrowUp -> "icon-arrow-up-double"
  IcnDoubleArrowDown -> "icon-arrow-down-double"
  IcnExpandShrinkArrowLeft -> "icon-expand-shrink-arrow-left"
  IcnExpandShrinkArrowRight -> "icon-expand-shrink-arrow-right"
  IcnReorder -> "icon-reorder"

iconDefOf :: Icon -> View m a
iconDefOf icn = MS.symbol_ [MSP.id_ $ iconId icn, MSP.viewBox_ "0 0 24 24"] (iconDefOf' icn)

iconDefOf' :: Icon -> [View m a]
iconDefOf' = \case
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
    [ MS.path_
        [ MSP.d_ "M19 15L12 9L5 15"
        , MSP.strokeWidth_ "1.5"
        , MSP.strokeLinecap_ "round"
        , MSP.strokeLinejoin_ "round"
        ]
    ]
  IcnArrowDown ->
    [ MS.path_
        [ MSP.d_ "M19 9L12 15L5 9"
        , MSP.strokeWidth_ "1.5"
        , MSP.strokeLinecap_ "round"
        , MSP.strokeLinejoin_ "round"
        ]
    ]
  IcnDoubleArrowUp ->
    [ MS.path_
        [ MSP.d_ "M19 13L12 7L5 13"
        , MSP.strokeWidth_ "1.5"
        , MSP.strokeLinecap_ "round"
        , MSP.strokeLinejoin_ "round"
        ]
    , MS.path_
        [ MSP.d_ "M19 17L12 11L5 17"
        , MSP.strokeWidth_ "1.5"
        , MSP.strokeLinecap_ "round"
        , MSP.strokeLinejoin_ "round"
        ]
    ]
  IcnDoubleArrowDown ->
    [ MS.path_
        [ MSP.d_ "M19 11L12 17L5 11"
        , MSP.strokeWidth_ "1.5"
        , MSP.strokeLinecap_ "round"
        , MSP.strokeLinejoin_ "round"
        ]
    , MS.path_
        [ MSP.d_ "M19 7L12 13L5 7"
        , MSP.strokeWidth_ "1.5"
        , MSP.strokeLinecap_ "round"
        , MSP.strokeLinejoin_ "round"
        ]
    ]
  -- todo: this is ai generated, look for proper icon!
  IcnExpandShrinkArrowLeft ->
    [ MS.path_
        [ MSP.d_ "M15 18L9 12L15 6"
        , MSP.strokeWidth_ "1.5"
        , MSP.strokeLinecap_ "round"
        , MSP.strokeLinejoin_ "round"
        ]
    ]
  -- todo: this is ai generated, look for proper icon!
  IcnExpandShrinkArrowRight ->
    [ MS.path_
        [ MSP.d_ "M9 18L15 12L9 6"
        , MSP.strokeWidth_ "1.5"
        , MSP.strokeLinecap_ "round"
        , MSP.strokeLinejoin_ "round"
        ]
    ]
  IcnReorder ->
    [ MS.path_
        [ MSP.d_ "M16 18L16 6M16 6L20 10.125M16 6L12 10.125"
        , MSP.strokeWidth_ "1.5"
        , MSP.strokeLinecap_ "round"
        , MSP.strokeLinejoin_ "round"
        ]
    , MS.path_
        [ MSP.d_ "M8 6L8 18M8 18L12 13.875M8 18L4 13.875"
        , MSP.strokeWidth_ "1.5"
        , MSP.strokeLinecap_ "round"
        , MSP.strokeLinejoin_ "round"
        ]
    ]
