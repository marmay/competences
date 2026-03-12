{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Frontend.Component.ImportModal
-- Description : Generic import modal component
--
-- Provides a reusable split-pane import modal with input textarea on the left
-- and preview on the right. Entity-specific behaviour is configured via
-- 'ImportModalConfig'.
module Competences.Frontend.Component.ImportModal
  ( -- * Configuration
    ImportModalConfig (..)

    -- * Component
  , importModalComponent

    -- * Shared view helpers
  , actionBadge

    -- * Re-exports for convenience
  , Model
  , Action (..)
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext (..)
  , subscribeDocument
  )
import Competences.Frontend.SyncContext.WindowManager (WindowMode)
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Import.Types (ImportAction (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Optics.Core ((.~))

-- ============================================================================
-- Configuration
-- ============================================================================

-- | Configuration for a specific import type.
--
-- Each entity module provides one of these to plug into the generic modal.
data ImportModalConfig preview = ImportModalConfig
  { parse :: Document -> Text -> Either String [preview]
  -- ^ Parse input text into preview items, given the current document.
  , renderItem :: preview -> M.View (Model preview) Action
  -- ^ Render a single preview item.
  , hasChanges :: preview -> Bool
  -- ^ Does this preview item represent an actual change?
  , apply :: SyncContext -> WindowMode -> Document -> [preview] -> IO ()
  -- ^ Apply the previewed changes (issue commands, then close window).
  , placeholder :: M.MisoString
  -- ^ Placeholder text shown in the textarea.
  }

-- ============================================================================
-- Model
-- ============================================================================

data Model preview = Model
  { inputText :: !Text
  , parseResult :: !(Either String [preview])
  , document :: !Document
  }
  deriving (Eq, Generic, Show)

-- ============================================================================
-- Actions
-- ============================================================================

data Action
  = DocumentUpdated !DocumentChange
  | SetInputText !Text
  | ParseInput
  | ApplyImport
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

importModalComponent
  :: ImportModalConfig preview
  -> SyncContext
  -> WindowMode
  -> M.Component p (Model preview) Action
importModalComponent cfg r wm =
  (M.component model update view)
    { M.subs = [subscribeDocument r DocumentUpdated]
    }
  where
    model =
      Model
        { inputText = ""
        , parseResult = Right []
        , document = emptyDocument
        }

    update (DocumentUpdated dc) =
      M.modify $ #document .~ dc.document

    update (SetInputText t) =
      M.modify $ #inputText .~ t

    update ParseInput = do
      m <- M.get
      let result = cfg.parse m.document m.inputText
      M.modify $ #parseResult .~ result

    update ApplyImport = do
      m <- M.get
      case m.parseResult of
        Right previews -> M.io_ $ cfg.apply r wm m.document previews
        Left _ -> pure ()

    view m =
      Layout.vFlow Layout.hFull
        [ -- Content
          Layout.scrollContent $ Layout.padM $
            Layout.hFlow (Layout.gapM <> Layout.hFull)
              [ -- Left: Input area
                MH.div_
                  [class_ "min-h-0 flex-1 w-1/2 h-full"]
                  [ Layout.vFlow (Layout.gapS <> Layout.hFull)
                      [ Typography.h3 "Eingabe"
                      , M.textarea_
                          [ class_ "flex-1 min-h-0 w-full p-3 font-mono text-sm border border-input rounded-md bg-background resize-none"
                          , MP.placeholder_ cfg.placeholder
                          , MP.value_ (M.ms m.inputText)
                          , M.onInput (SetInputText . M.fromMisoString)
                          ]
                          []
                      ]
                  ]
              , -- Right: Preview area
                MH.div_
                  [class_ "min-h-0 flex-1 w-1/2 h-full"]
                  [ Layout.vFlow (Layout.gapS <> Layout.hFull)
                      [ Typography.h3 "Vorschau"
                      , M.div_
                          [class_ "flex-1 min-h-0 overflow-y-auto border border-border rounded-md p-3 bg-muted/30"]
                          [previewView cfg m]
                      ]
                  ]
              ]
        , Layout.actionFooter
            [ Button.primary (Button.button ("Vorschau" :: M.MisoString) ParseInput)
            , case m.parseResult of
                Right previews
                  | not (null previews) && any cfg.hasChanges previews ->
                      Button.applyButton ApplyImport
                _ -> M.text ""
            ]
        ]

-- ============================================================================
-- Preview View
-- ============================================================================

previewView :: ImportModalConfig preview -> Model preview -> M.View (Model preview) Action
previewView cfg m = case m.parseResult of
  Left err ->
    M.div_
      [class_ "text-destructive"]
      [M.text $ M.ms $ "Fehler: " <> err]
  Right [] ->
    M.div_
      [class_ "text-muted-foreground italic"]
      [M.text "Keine Eingabe. Geben Sie Text ein und klicken Sie auf 'Vorschau'."]
  Right previews ->
    Layout.vFlow Layout.gapM
      (map cfg.renderItem previews)

-- ============================================================================
-- Shared Helpers
-- ============================================================================

actionBadge :: ImportAction a -> M.View model action
actionBadge (Create _) = Badge.primary (Badge.badgeText "Neu")
actionBadge (Update _ _) = Badge.secondary (Badge.badgeText "Aktualisiert")
actionBadge (NoChange _) = Badge.outline (Badge.badgeText "Unverändert")

emptyDocument :: Document
emptyDocument =
  Document
    { competenceGrids = Ix.empty
    , competences = Ix.empty
    , users = Ix.empty
    , evidences = Ix.empty
    , locks = mempty
    , tasks = Ix.empty
    , taskGroups = Ix.empty
    , solutions = Ix.empty
    , resources = Ix.empty
    , assignments = Ix.empty
    , competenceAssessments = Ix.empty
    , competenceGridGrades = Ix.empty
    , mesoPlans = Ix.empty
    , lessons = Ix.empty
    , lessonNotes = Ix.empty
    , participationRecords = Ix.empty
    , absences = Ix.empty
    , submissions = Ix.empty
    , draftTasks = Ix.empty
    , draftTaskGroups = Ix.empty
    , draftAssignments = Ix.empty
    }
