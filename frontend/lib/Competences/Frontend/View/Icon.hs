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
import Miso.Html.Property qualified as M
import Miso.String (MisoString)
import Miso.Svg qualified as MS
import Miso.Svg.Property qualified as MSP

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
  | IcnActivityTypeSupervised
  | IcnActivityTypeSemiSupervised
  | IcnActivityTypeUnsupervised
  | IcnSocialFormIndividual
  | IcnSocialFormGroup
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
  IcnActivityTypeSupervised -> "icon-activity-type-supervised"
  IcnActivityTypeSemiSupervised -> "icon-activity-type-semi-supervised"
  IcnActivityTypeUnsupervised -> "icon-activity-type-unsupervised"
  IcnSocialFormIndividual -> "icon-social-form-individual"
  IcnSocialFormGroup -> "icon-social-form-group"

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
  IcnActivityTypeSupervised ->
    map
      (\s -> MS.path_ [MSP.d_ s, MSP.strokeWidth_ "1.5", MSP.fill_ "none"])
      [ "M5 3m0 2a2 2 0 0 1 2 -2h10a2 2 0 0 1 2 2v14a2 2 0 0 1 -2 2h-10a2 2 0 0 1 -2 -2z"
      , "M9 7l6 0"
      , "M9 11l6 0"
      , "M9 15l4 0"
      ]
  IcnActivityTypeSemiSupervised ->
    [ MS.path_
        [ MSP.d_
            "M6 11.4999H7M6 15.4999H7M17 15.4999H18M17 11.4999H18M11.5 11.4999H12.5M10 20.9999V16.9999C10 15.8954 10.8954 14.9999 12 14.9999C13.1046 14.9999 14 15.8954 14 16.9999V20.9999M17 7.49995L18.5761 7.89398C19.4428 8.11064 19.8761 8.21898 20.1988 8.46057C20.4834 8.67373 20.7061 8.95895 20.8439 9.28682C21 9.65843 21 10.1051 21 10.9984V17.7999C21 18.9201 21 19.4801 20.782 19.9079C20.5903 20.2843 20.2843 20.5902 19.908 20.782C19.4802 20.9999 18.9201 20.9999 17.8 20.9999H6.2C5.0799 20.9999 4.51984 20.9999 4.09202 20.782C3.71569 20.5902 3.40973 20.2843 3.21799 19.9079C3 19.4801 3 18.9201 3 17.7999V10.9984C3 10.1051 3 9.65843 3.15613 9.28682C3.29388 8.95895 3.51657 8.67373 3.80124 8.46057C4.12389 8.21898 4.55722 8.11064 5.42388 7.89398L7 7.49995L9.85931 4.92657C10.6159 4.2456 10.9943 3.90512 11.4221 3.77598C11.799 3.66224 12.201 3.66224 12.5779 3.77598C13.0057 3.90512 13.3841 4.2456 14.1407 4.92657L17 7.49995Z"
        , MSP.strokeWidth_ "1.5"
        , MSP.fill_ "none"
        ]
    ]
  IcnActivityTypeUnsupervised ->
    [ MS.path_
        [ MSP.d_
            "M8 15.0001C8 15.0001 9.6 17.0001 12 17.0001C14.4 17.0001 16 15.0001 16 15.0001M3 14.6001V12.1302C3 10.9815 3 10.4071 3.14805 9.87819C3.2792 9.40966 3.49473 8.96898 3.78405 8.5778C4.11067 8.1362 4.56404 7.78358 5.47078 7.07834L8.07078 5.05612C9.47608 3.96311 10.1787 3.4166 10.9546 3.20653C11.6392 3.02116 12.3608 3.02116 13.0454 3.20653C13.8213 3.4166 14.5239 3.96311 15.9292 5.05612L18.5292 7.07834C19.436 7.78358 19.8893 8.1362 20.2159 8.5778C20.5053 8.96898 20.7208 9.40966 20.8519 9.87819C21 10.4071 21 10.9815 21 12.1302V14.6001C21 16.8403 21 17.9604 20.564 18.816C20.1805 19.5687 19.5686 20.1806 18.816 20.5641C17.9603 21.0001 16.8402 21.0001 14.6 21.0001H9.4C7.15979 21.0001 6.03969 21.0001 5.18404 20.5641C4.43139 20.1806 3.81947 19.5687 3.43597 18.816C3 17.9604 3 16.8403 3 14.6001Z"
        , MSP.strokeWidth_ "1.5"
        , MSP.fill_ "none"
        ]
    ]
  IcnSocialFormIndividual ->
    [ MS.path_
        [ MSP.d_
            "M12 1.25C9.37665 1.25 7.25 3.37665 7.25 6C7.25 8.62335 9.37665 10.75 12 10.75C14.6234 10.75 16.75 8.62335 16.75 6C16.75 3.37665 14.6234 1.25 12 1.25ZM8.75 6C8.75 4.20507 10.2051 2.75 12 2.75C13.7949 2.75 15.25 4.20507 15.25 6C15.25 7.79493 13.7949 9.25 12 9.25C10.2051 9.25 8.75 7.79493 8.75 6Z"
        , MSP.strokeWidth_ "1.5"
        , MSP.fillRule_ "evenodd"
        , MSP.clipRule_ "evenodd"
        ]
    , MS.path_
        [ MSP.d_
            "M12 12.25C9.96067 12.25 8.07752 12.7208 6.67815 13.5204C5.3 14.3079 4.25 15.5101 4.25 17C4.25 18.4899 5.3 19.6921 6.67815 20.4796C8.07752 21.2792 9.96067 21.75 12 21.75C14.0393 21.75 15.9225 21.2792 17.3219 20.4796C18.7 19.6921 19.75 18.4899 19.75 17C19.75 15.5101 18.7 14.3079 17.3219 13.5204C15.9225 12.7208 14.0393 12.25 12 12.25ZM5.75 17C5.75 16.2807 6.26701 15.483 7.42236 14.8228C8.55649 14.1747 10.1733 13.75 12 13.75C13.8267 13.75 15.4435 14.1747 16.5776 14.8228C17.733 15.483 18.25 16.2807 18.25 17C18.25 17.7193 17.733 18.517 16.5776 19.1772C15.4435 19.8253 13.8267 20.25 12 20.25C10.1733 20.25 8.55649 19.8253 7.42236 19.1772C6.26701 18.517 5.75 17.7193 5.75 17Z"
        , MSP.strokeWidth_ "1.5"
        , MSP.fillRule_ "evenodd"
        , MSP.clipRule_ "evenodd"
        ]
    ]
  IcnSocialFormGroup ->
    map (\s -> MS.path_ [ MSP.d_ s, MSP.strokeWidth_ "1.5", MSP.fill_ "none" ])
      [ "M10 13a2 2 0 1 0 4 0a2 2 0 0 0 -4 0"
      , "M8 21v-1a2 2 0 0 1 2 -2h4a2 2 0 0 1 2 2v1"
      , "M15 5a2 2 0 1 0 4 0a2 2 0 0 0 -4 0"
      , "M17 10h2a2 2 0 0 1 2 2v1"
      , "M5 5a2 2 0 1 0 4 0a2 2 0 0 0 -4 0"
      , "M3 13v-1a2 2 0 0 1 2 -2h2"
      ]
    
