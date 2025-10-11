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
  IcnSocialFormIndividual ->
    [ MS.path_
        [ MSP.d_ "M12 1.25C9.37665 1.25 7.25 3.37665 7.25 6C7.25 8.62335 9.37665 10.75 12 10.75C14.6234 10.75 16.75 8.62335 16.75 6C16.75 3.37665 14.6234 1.25 12 1.25ZM8.75 6C8.75 4.20507 10.2051 2.75 12 2.75C13.7949 2.75 15.25 4.20507 15.25 6C15.25 7.79493 13.7949 9.25 12 9.25C10.2051 9.25 8.75 7.79493 8.75 6Z"
        , MSP.strokeWidth_ "1.5"
        , MSP.fillRule_ "evenodd"
        , MSP.clipRule_ "evenodd"
        ]
    , MS.path_
        [ MSP.d_ "M12 12.25C9.96067 12.25 8.07752 12.7208 6.67815 13.5204C5.3 14.3079 4.25 15.5101 4.25 17C4.25 18.4899 5.3 19.6921 6.67815 20.4796C8.07752 21.2792 9.96067 21.75 12 21.75C14.0393 21.75 15.9225 21.2792 17.3219 20.4796C18.7 19.6921 19.75 18.4899 19.75 17C19.75 15.5101 18.7 14.3079 17.3219 13.5204C15.9225 12.7208 14.0393 12.25 12 12.25ZM5.75 17C5.75 16.2807 6.26701 15.483 7.42236 14.8228C8.55649 14.1747 10.1733 13.75 12 13.75C13.8267 13.75 15.4435 14.1747 16.5776 14.8228C17.733 15.483 18.25 16.2807 18.25 17C18.25 17.7193 17.733 18.517 16.5776 19.1772C15.4435 19.8253 13.8267 20.25 12 20.25C10.1733 20.25 8.55649 19.8253 7.42236 19.1772C6.26701 18.517 5.75 17.7193 5.75 17Z"
        , MSP.strokeWidth_ "1.5"
        , MSP.fillRule_ "evenodd"
        , MSP.clipRule_ "evenodd"
        ]
    ]
  IcnSocialFormGroup ->
    [ MS.path_
        [ MSP.d_ "M12 1.25C9.37665 1.25 7.25 3.37665 7.25 6C7.25 8.62335 9.37665 10.75 12 10.75C14.6234 10.75 16.75 8.62335 16.75 6C16.75 3.37665 14.6234 1.25 12 1.25ZM8.75 6C8.75 4.20507 10.2051 2.75 12 2.75C13.7949 2.75 15.25 4.20507 15.25 6C15.25 7.79493 13.7949 9.25 12 9.25C10.2051 9.25 8.75 7.79493 8.75 6Z"
        , MSP.strokeWidth_ "1.5"
        , MSP.fillRule_ "evenodd"
        , MSP.clipRule_ "evenodd"
        ]
    , MS.path_
        [ MSP.d_ "M12 12.25C10.2157 12.25 8.56645 12.7308 7.34133 13.5475C6.12146 14.3608 5.25 15.5666 5.25 17C5.25 18.4334 6.12146 19.6392 7.34133 20.4525C8.56645 21.2692 10.2157 21.75 12 21.75C13.7843 21.75 15.4335 21.2692 16.6587 20.4525C17.8785 19.6392 18.75 18.4334 18.75 17C18.75 15.5666 17.8785 14.3608 16.6587 13.5475C15.4335 12.7308 13.7843 12.25 12 12.25ZM6.75 17C6.75 16.2242 7.22169 15.4301 8.17338 14.7956C9.11984 14.1646 10.4706 13.75 12 13.75C13.5294 13.75 14.8802 14.1646 15.8266 14.7956C16.7783 15.4301 17.25 16.2242 17.25 17C17.25 17.7758 16.7783 18.5699 15.8266 19.2044C14.8802 19.8354 13.5294 20.25 12 20.25C10.4706 20.25 9.11984 19.8354 8.17338 19.2044C7.22169 18.5699 6.75 17.7758 6.75 17Z"
        , MSP.strokeWidth_ "1.5"
        , MSP.fillRule_ "evenodd"
        , MSP.clipRule_ "evenodd"
        ]
    , MS.path_
        [ MSP.d_ "M18 3.25C17.5858 3.25 17.25 3.58579 17.25 4C17.25 4.41421 17.5858 4.75 18 4.75C19.3765 4.75 20.25 5.65573 20.25 6.5C20.25 7.34427 19.3765 8.25 18 8.25C17.5858 8.25 17.25 8.58579 17.25 9C17.25 9.41421 17.5858 9.75 18 9.75C19.9372 9.75 21.75 8.41715 21.75 6.5C21.75 4.58285 19.9372 3.25 18 3.25Z"
        ]
    , MS.path_
        [ MSP.d_ "M6.75 4C6.75 3.58579 6.41421 3.25 6 3.25C4.06278 3.25 2.25 4.58285 2.25 6.5C2.25 8.41715 4.06278 9.75 6 9.75C6.41421 9.75 6.75 9.41421 6.75 9C6.75 8.58579 6.41421 8.25 6 8.25C4.62351 8.25 3.75 7.34427 3.75 6.5C3.75 5.65573 4.62351 4.75 6 4.75C6.41421 4.75 6.75 4.41421 6.75 4Z"
        ]
    , MS.path_
        [ MSP.d_ "M19.2674 13.8393C19.3561 13.4347 19.7561 13.1787 20.1607 13.2674C21.1225 13.4783 21.9893 13.8593 22.6328 14.3859C23.2758 14.912 23.75 15.6352 23.75 16.5C23.75 17.3648 23.2758 18.088 22.6328 18.6141C21.9893 19.1407 21.1225 19.5217 20.1607 19.7326C19.7561 19.8213 19.3561 19.5653 19.2674 19.1607C19.1787 18.7561 19.4347 18.3561 19.8393 18.2674C20.6317 18.0936 21.2649 17.7952 21.6829 17.4532C22.1014 17.1108 22.25 16.7763 22.25 16.5C22.25 16.2237 22.1014 15.8892 21.6829 15.5468C21.2649 15.2048 20.6317 14.9064 19.8393 14.7326C19.4347 14.6439 19.1787 14.2439 19.2674 13.8393Z"
        ]
    , MS.path_
        [ MSP.d_ "M3.83935 13.2674C4.24395 13.1787 4.64387 13.4347 4.73259 13.8393C4.82132 14.2439 4.56525 14.6439 4.16065 14.7326C3.36829 14.9064 2.73505 15.2048 2.31712 15.5468C1.89863 15.8892 1.75 16.2237 1.75 16.5C1.75 16.7763 1.89863 17.1108 2.31712 17.4532C2.73505 17.7952 3.36829 18.0936 4.16065 18.2674C4.56525 18.3561 4.82132 18.7561 4.73259 19.1607C4.64387 19.5653 4.24395 19.8213 3.83935 19.7326C2.87746 19.5217 2.0107 19.1407 1.36719 18.6141C0.724248 18.088 0.25 17.3648 0.25 16.5C0.25 15.6352 0.724248 14.912 1.36719 14.3859C2.0107 13.8593 2.87746 13.4783 3.83935 13.2674Z"
        ]
    ]
    
