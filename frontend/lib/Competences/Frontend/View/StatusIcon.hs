-- | Stateless status icon rendering for competence grid cells.
--
-- Provides a 'Status' type that abstracts achievement/progress/lock state,
-- and rendering functions for different layout contexts (centered, overlay, inline).
module Competences.Frontend.View.StatusIcon
  ( Status (..)
  , statusIcon
  , statusIconOverlay
  , lockIcon
  )
where

import Competences.Frontend.View.Icon (Icon (..), icon)
import Competences.Frontend.View.Tailwind (class_)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as MP

-- | Visual status of an item (e.g. competence level cell).
-- Components map their domain state to this type.
data Status
  = Achieved
  | InProgress
  | Locked
  | NoStatus
  deriving (Eq, Show)

-- | Render a status icon, centered in its container (for grid cells in Grading view).
statusIcon :: Status -> M.View m a
statusIcon Achieved =
  M.div_
    [class_ "text-green-600 flex justify-center"]
    [icon [MP.width_ "16", MP.height_ "16"] IcnApply]
statusIcon InProgress =
  M.div_
    [class_ "text-yellow-600 flex justify-center"]
    [icon [MP.width_ "16", MP.height_ "16"] IcnProgress]
statusIcon Locked =
  M.div_
    [class_ "text-stone-500 flex justify-center"]
    [icon [MP.width_ "16", MP.height_ "16"] IcnLock]
statusIcon NoStatus = M.text ""

-- | Render a status icon as an absolute-positioned overlay (top-right corner).
-- The parent element must have @position: relative@.
statusIconOverlay :: Status -> M.View m a
statusIconOverlay Achieved =
  M.div_
    [class_ "absolute top-1 right-1 text-green-600"]
    [icon [MP.width_ "14", MP.height_ "14"] IcnApply]
statusIconOverlay InProgress =
  M.div_
    [class_ "absolute top-1 right-1 text-yellow-600"]
    [icon [MP.width_ "14", MP.height_ "14"] IcnProgress]
statusIconOverlay Locked =
  M.div_
    [class_ "absolute top-1 right-1 text-stone-500"]
    [icon [MP.width_ "14", MP.height_ "14"] IcnLock]
statusIconOverlay NoStatus = M.text ""

-- | Small inline lock icon for use in level labels and descriptions.
lockIcon :: M.View m a
lockIcon =
  M.span_
    [class_ "text-stone-400"]
    [icon [MP.width_ "14", MP.height_ "14"] IcnLock]
