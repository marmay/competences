-- | Re-exports View modules for convenience.
--
-- Note: Icon and Disclosure modules are NOT re-exported here because their
-- constructors conflict with Button's. Import them qualified:
--
-- > import Competences.Frontend.View.Icon qualified as Icon
-- > import Competences.Frontend.View.Disclosure qualified as Disclosure
module Competences.Frontend.View
  ( module Competences.Frontend.View.Button
  , module Competences.Frontend.View.CellStyle
  , module Competences.Frontend.View.Color
  , module Competences.Frontend.View.Combobox
  , module Competences.Frontend.View.EvidenceIcon
  , inlineComponent
  , inlineComponentAttrs
  , module Competences.Frontend.View.Form
  , module Competences.Frontend.View.Layout
  , module Competences.Frontend.View.MainPage
  , module Competences.Frontend.View.SidePanel
  , module Competences.Frontend.View.StatusIcon
  , module Competences.Frontend.View.Table
  , module Competences.Frontend.View.TagInput
  , module Competences.Frontend.View.Text
  , module Competences.Frontend.View.Tooltip
  )
  where

import Competences.Frontend.View.Button
import Competences.Frontend.View.CellStyle
import Competences.Frontend.View.Color
import Competences.Frontend.View.Combobox
import Competences.Frontend.View.EvidenceIcon
import Competences.Frontend.SyncContext.WindowManager (inlineComponent, inlineComponentAttrs)
import Competences.Frontend.View.Form
import Competences.Frontend.View.Layout
import Competences.Frontend.View.MainPage
import Competences.Frontend.View.SidePanel
import Competences.Frontend.View.StatusIcon
import Competences.Frontend.View.Table
import Competences.Frontend.View.TagInput
import Competences.Frontend.View.Text
import Competences.Frontend.View.Tooltip
