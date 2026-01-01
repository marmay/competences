{- |
Module: Competences.Frontend.View.Input
Description: Basecoat-inspired form input components

This module provides form input components following Basecoat design patterns.
-}
module Competences.Frontend.View.Input
  ( -- * Input fields
    textInput
  , textInput'
  , passwordInput
  , emailInput
  , numberInput
  , dateInput
  , textarea
  , textarea'

    -- * Labels and field wrappers
  , label
  , label'
  , fieldWrapper
  , fieldWrapperHorizontal
  , helperText

    -- * Input configuration
  , InputConfig (..)
  , defaultInput
  , withPlaceholder
  , withValue
  , withDisabled
  , withInvalid
  , withOnInput
  , renderInput
  )
where

import Competences.Frontend.View.Tailwind (class_)
import qualified Data.Text as Text
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as MP
import Miso.String (MisoString, ms)

-- ============================================================================
-- INPUT CONFIGURATION
-- ============================================================================

-- | Configuration for form inputs
data InputConfig model action = InputConfig
  { inputType :: !MisoString
  , placeholder :: !(Maybe MisoString)
  , value :: !MisoString
  , disabled :: !Bool
  , invalid :: !Bool
  , attrs :: ![M.Attribute action]
  , onInput :: !(Maybe (MisoString -> action))
  }

-- | Default input configuration
defaultInput :: InputConfig model action
defaultInput = InputConfig
  { inputType = "text"
  , placeholder = Nothing
  , value = ""
  , disabled = False
  , invalid = False
  , attrs = []
  , onInput = Nothing
  }

-- | Add placeholder text
withPlaceholder :: MisoString -> InputConfig m action -> InputConfig m action
withPlaceholder p (InputConfig t _ v d i as oi) = InputConfig t (Just p) v d i as oi

-- | Set input value
withValue :: MisoString -> InputConfig m action -> InputConfig m action
withValue v (InputConfig t p _ d i as oi) = InputConfig t p v d i as oi

-- | Set disabled state
withDisabled :: Bool -> InputConfig m action -> InputConfig m action
withDisabled d (InputConfig t p v _ i as oi) = InputConfig t p v d i as oi

-- | Set invalid state
withInvalid :: Bool -> InputConfig m action -> InputConfig m action
withInvalid i (InputConfig t p v d _ as oi) = InputConfig t p v d i as oi

-- | Set input handler
withOnInput :: (MisoString -> action) -> InputConfig m action -> InputConfig m action
withOnInput handler (InputConfig t p v d i as _) = InputConfig t p v d i as (Just handler)

-- | Render an input field
renderInput :: InputConfig model action -> M.View model action
renderInput cfg =
  M.input_
    ( [ class_ $ Text.unwords
          [ baseClasses
          , if cfg.invalid then invalidClasses else ""
          ]
      , MP.type_ cfg.inputType
      , MP.value_ cfg.value
      ] <> maybe [] (\p -> [MP.placeholder_ p]) cfg.placeholder
        <> (if cfg.disabled then [MP.disabled_] else [])
        <> maybe [] (\h -> [M.onInput h]) cfg.onInput
        <> cfg.attrs
    )
  where
    -- Base input classes from Basecoat
    baseClasses = "appearance-none h-9 w-full rounded-md border border-stone-300 bg-transparent px-3 py-1 shadow-xs focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-sky-600 disabled:opacity-50 disabled:cursor-not-allowed placeholder:text-stone-500 selection:bg-sky-600"

    invalidClasses = "border-red-600 focus-visible:ring-red-600"

-- ============================================================================
-- CONVENIENT INPUT CONSTRUCTORS
-- ============================================================================

-- | Text input field
textInput :: MisoString -- ^ Value
          -> (MisoString -> action) -- ^ On input handler
          -> M.View model action
textInput val handler =
  renderInput $ defaultInput
    & withValue val
    & withOnInput handler

-- | Text input with placeholder
textInput' :: MisoString -- ^ Placeholder
           -> MisoString -- ^ Value
           -> (MisoString -> action) -- ^ On input handler
           -> M.View model action
textInput' ph val handler =
  renderInput $ defaultInput
    & withPlaceholder ph
    & withValue val
    & withOnInput handler

-- | Password input field
passwordInput :: MisoString -- ^ Value
              -> (MisoString -> action) -- ^ On input handler
              -> M.View model action
passwordInput val handler =
  renderInput $ InputConfig
    { inputType = "password"
    , placeholder = Nothing
    , value = val
    , disabled = False
    , invalid = False
    , attrs = []
    , onInput = Just handler
    }

-- | Email input field
emailInput :: MisoString -- ^ Value
           -> (MisoString -> action) -- ^ On input handler
           -> M.View model action
emailInput val handler =
  renderInput $ InputConfig
    { inputType = "email"
    , placeholder = Nothing
    , value = val
    , disabled = False
    , invalid = False
    , attrs = []
    , onInput = Just handler
    }

-- | Number input field
numberInput :: MisoString -- ^ Value
            -> (MisoString -> action) -- ^ On input handler
            -> M.View model action
numberInput val handler =
  renderInput $ InputConfig
    { inputType = "number"
    , placeholder = Nothing
    , value = val
    , disabled = False
    , invalid = False
    , attrs = []
    , onInput = Just handler
    }

-- | Date input field
dateInput :: MisoString -- ^ Value (YYYY-MM-DD format)
          -> (MisoString -> action) -- ^ On input handler
          -> M.View model action
dateInput val handler =
  renderInput $ InputConfig
    { inputType = "date"
    , placeholder = Nothing
    , value = val
    , disabled = False
    , invalid = False
    , attrs = []
    , onInput = Just handler
    }

-- | Textarea field with configuration
textarea' :: Int -- ^ Rows
          -> MisoString -- ^ Value
          -> (MisoString -> action) -- ^ On input handler
          -> M.View model action
textarea' rows val handler =
  M.textarea_
    [ class_ baseClasses
    , MP.rows_ $ ms $ Text.pack $ show rows
    , MP.value_ val
    , M.onInput handler
    ]
    []
  where
    baseClasses = "border-stone-300 w-full rounded-md border bg-transparent px-3 py-2 shadow-xs focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-sky-600 placeholder:text-stone-500 disabled:opacity-50"

-- | Textarea field with default 4 rows
textarea :: MisoString -- ^ Value
         -> (MisoString -> action) -- ^ On input handler
         -> M.View model action
textarea = textarea' 4

-- ============================================================================
-- LABELS AND FIELD WRAPPERS
-- ============================================================================

-- | Label for form field
label :: MisoString -> M.View model action
label text =
  M.label_
    [class_ "flex items-center gap-2 text-sm leading-none font-medium select-none"]
    [M.text text]

-- | Label with custom attributes
label' :: [M.Attribute action] -> MisoString -> M.View model action
label' attrs text =
  M.label_
    (class_ "flex items-center gap-2 text-sm leading-none font-medium select-none" : attrs)
    [M.text text]

-- | Vertical field wrapper (label above input)
fieldWrapper :: MisoString -- ^ Label text
             -> M.View model action -- ^ Input element
             -> M.View model action
fieldWrapper labelText input =
  M.div_
    [class_ "flex flex-col w-full gap-3"]
    [ label labelText
    , input
    ]

-- | Horizontal field wrapper (label beside input)
fieldWrapperHorizontal :: MisoString -- ^ Label text
                       -> M.View model action -- ^ Input element
                       -> M.View model action
fieldWrapperHorizontal labelText input =
  M.div_
    [class_ "flex flex-row items-center w-full gap-3"]
    [ label labelText
    , input
    ]

-- | Helper text below input
helperText :: MisoString -> M.View model action
helperText text =
  M.p_
    [class_ "text-stone-500 text-sm"]
    [M.text text]

-- ============================================================================
-- HELPER for chaining withX functions
-- ============================================================================

(&) :: a -> (a -> a) -> a
x & f = f x
