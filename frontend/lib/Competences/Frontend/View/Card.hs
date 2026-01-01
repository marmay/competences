{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Competences.Frontend.View.Card
Description: Basecoat-inspired card/panel components

This module provides card and panel components following Basecoat design patterns.
-}
module Competences.Frontend.View.Card
  ( -- * Card components
    card
  , cardWithHeader
  , cardWithFooter
  , cardFull

    -- * Card sections
  , cardHeader
  , cardContent
  , cardFooter
  )
where

import Competences.Frontend.View.Tailwind (class_)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (MisoString)

-- | Simple card with content only
card :: [M.View model action] -> M.View model action
card content =
  M.div_
    [class_ cardClasses]
    [M.section_ [class_ "px-6"] content]
  where
    cardClasses = "bg-white text-stone-900 flex flex-col gap-6 rounded-xl border border-stone-200 py-6 shadow-sm"

-- | Card with header and content
cardWithHeader :: MisoString -- ^ Title
               -> Maybe MisoString -- ^ Optional description
               -> [M.View model action] -- ^ Content
               -> M.View model action
cardWithHeader title desc content =
  M.div_
    [class_ cardClasses]
    [ cardHeader title desc
    , M.section_ [class_ "px-6"] content
    ]
  where
    cardClasses = "bg-white text-stone-900 flex flex-col gap-6 rounded-xl border border-stone-200 py-6 shadow-sm"

-- | Card with footer for actions
cardWithFooter :: [M.View model action] -- ^ Content
               -> [M.View model action] -- ^ Footer content (typically buttons)
               -> M.View model action
cardWithFooter content footer =
  M.div_
    [class_ cardClasses]
    [ M.section_ [class_ "px-6"] content
    , cardFooter footer
    ]
  where
    cardClasses = "bg-white text-stone-900 flex flex-col gap-6 rounded-xl border border-stone-200 py-6 shadow-sm"

-- | Full card with header, content, and footer
cardFull :: MisoString -- ^ Title
         -> Maybe MisoString -- ^ Optional description
         -> [M.View model action] -- ^ Content
         -> [M.View model action] -- ^ Footer content
         -> M.View model action
cardFull title desc content footer =
  M.div_
    [class_ cardClasses]
    [ cardHeader title desc
    , M.section_ [class_ "px-6"] content
    , cardFooter footer
    ]
  where
    cardClasses = "bg-white text-stone-900 flex flex-col gap-6 rounded-xl border border-stone-200 py-6 shadow-sm"

-- | Card header with title and optional description
cardHeader :: MisoString -> Maybe MisoString -> M.View model action
cardHeader title desc =
  M.header_
    [class_ "grid grid-rows-[auto_auto] items-start gap-1.5 px-6"]
    $ M.h2_ [class_ "text-lg font-semibold"] [M.text title]
    : maybe [] (\d -> [M.p_ [class_ "text-sm text-stone-500"] [M.text d]]) desc

-- | Card content section
cardContent :: [M.View model action] -> M.View model action
cardContent content =
  M.section_ [class_ "px-6"] content

-- | Card footer for actions
cardFooter :: [M.View model action] -> M.View model action
cardFooter content =
  M.footer_ [class_ "flex items-center px-6 gap-2"] content
