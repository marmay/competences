{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Frontend.Component.CompetenceGridImport
-- Description : Bulk import component for competence grids
--
-- Provides a UI for importing competence grids from a markdown-like format.
-- Shows a preview of changes (create/update/no-change) before applying.
module Competences.Frontend.Component.CompetenceGridImport
  ( competenceGridImportComponent
  )
where

import Competences.Command qualified as Cmd
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Document.Competence (Competence (..), Level (..))
import Competences.Document.CompetenceGrid (CompetenceGrid (..))
import Competences.Document.Id (Id (..))
import Competences.Document.Order (orderMax)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.View.Badge (BadgeVariant (..), badge)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Card qualified as Card
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Competences.Import.CompetenceGridParser (parseGridImport)
import Competences.Import.Matching (matchGridImport)
import Competences.Import.Types
  ( CompetenceImportAction (..)
  , GridImportPreview (..)
  , ImportAction (..)
  , ParsedCompetence (..)
  , levelToGerman
  )
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as MP
import Optics.Core ((&), (.~))

-- ============================================================================
-- Model
-- ============================================================================

data Model = Model
  { inputText :: !Text
  , parseResult :: !(Either String [GridImportPreview])
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

competenceGridImportComponent :: SyncContext -> M.Component p Model Action
competenceGridImportComponent r =
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
        }

    update (DocumentUpdated dc) =
      M.modify $ #document .~ dc.document

    update (SetInputText t) =
      M.modify $ #inputText .~ t

    update ParseInput = do
      m <- M.get
      let result = case parseGridImport m.inputText of
            Left err -> Left err
            Right parsed -> Right $ matchGridImport m.document parsed
      M.modify $ #parseResult .~ result

    update ApplyImport = do
      m <- M.get
      case m.parseResult of
        Right previews -> M.io_ $ applyPreviews r m.document previews
        Left _ -> pure ()

    view :: Model -> M.View Model Action
    view m =
      M.div_
        [class_ "h-full flex flex-col gap-4 p-4"]
        [ Typography.h2 (C.translate' C.LblImportCompetenceGrids)
        , M.div_
            [class_ "flex-1 min-h-0 grid grid-cols-2 gap-4"]
            [ -- Left: Input area
              M.div_
                [class_ "flex flex-col gap-2"]
                [ Typography.h3 "Eingabe"
                , M.textarea_
                    [ class_ "flex-1 min-h-0 w-full p-3 font-mono text-sm border border-input rounded-md bg-background resize-none"
                    , MP.placeholder_ "# Rastername\n\n## Kompetenzbeschreibung\n- Wesentlich: ...\n- Mittelstufe: ...\n- Fortgeschritten: ..."
                    , MP.value_ (M.ms m.inputText)
                    , M.onInput (SetInputText . M.fromMisoString)
                    ]
                    []
                , M.div_
                    [class_ "flex gap-2"]
                    [ Button.buttonPrimary "Vorschau"
                        & Button.withClick ParseInput
                        & Button.renderButton
                    ]
                ]
            , -- Right: Preview area
              M.div_
                [class_ "flex flex-col gap-2 min-h-0"]
                [ Typography.h3 "Vorschau"
                , M.div_
                    [class_ "flex-1 min-h-0 overflow-y-auto border border-border rounded-md p-3 bg-muted/30"]
                    [previewView m]
                , M.div_
                    [class_ "flex gap-2 justify-end"]
                    [ case m.parseResult of
                        Right previews
                          | not (null previews) && any hasChanges previews ->
                              Button.buttonPrimary (C.translate' C.LblApply)
                                & Button.withIcon IcnApply
                                & Button.withClick ApplyImport
                                & Button.renderButton
                        _ -> M.text ""
                    ]
                ]
            ]
        ]

-- ============================================================================
-- Preview View
-- ============================================================================

previewView :: Model -> M.View Model Action
previewView m = case m.parseResult of
  Left err ->
    M.div_
      [class_ "text-destructive"]
      [M.text $ M.ms $ "Fehler: " <> err]
  Right [] ->
    M.div_
      [class_ "text-muted-foreground italic"]
      [M.text "Keine Eingabe. Geben Sie Text ein und klicken Sie auf 'Vorschau'."]
  Right previews ->
    M.div_
      [class_ "flex flex-col gap-4"]
      (map previewGridView previews)

previewGridView :: GridImportPreview -> M.View Model Action
previewGridView preview =
  Card.card
    [ M.div_
        [class_ "flex items-center gap-2 mb-2"]
        [ Typography.h4 $ M.ms $ gridTitle preview.gridAction
        , actionBadge preview.gridAction
        ]
    , M.div_
        [class_ "flex flex-col gap-2"]
        (map previewCompetenceView preview.competenceActions)
    ]

gridTitle :: ImportAction CompetenceGrid -> Text
gridTitle (Create g) = g.title
gridTitle (Update _ g) = g.title
gridTitle (NoChange g) = g.title

previewCompetenceView :: CompetenceImportAction -> M.View Model Action
previewCompetenceView ca =
  M.div_
    [class_ "pl-4 border-l-2 border-border"]
    [ M.div_
        [class_ "flex items-center gap-2"]
        [ M.span_
            [class_ "font-medium"]
            [M.text $ M.ms ca.parsedCompetence.description]
        , actionBadge ca.action
        ]
    , M.div_
        [class_ "text-sm text-muted-foreground mt-1"]
        (levelPreview ca.parsedCompetence.levels)
    ]

levelPreview :: Map.Map Level Text -> [M.View Model Action]
levelPreview levels =
  map levelItem [BasicLevel, IntermediateLevel, AdvancedLevel]
  where
    levelItem lvl = case Map.lookup lvl levels of
      Nothing -> M.text ""
      Just desc ->
        M.div_
          []
          [ M.span_
              [class_ "font-medium"]
              [M.text $ M.ms $ levelToGerman lvl <> ": "]
          , M.text $ M.ms $ T.take 60 desc <> if T.length desc > 60 then "..." else ""
          ]

actionBadge :: ImportAction a -> M.View Model Action
actionBadge (Create _) = badge BadgePrimary "Neu"
actionBadge (Update _ _) = badge BadgeSecondary "Aktualisiert"
actionBadge (NoChange _) = badge BadgeOutline "Unverändert"

-- ============================================================================
-- Apply Import
-- ============================================================================

hasChanges :: GridImportPreview -> Bool
hasChanges preview =
  isChange preview.gridAction
    || any (\ca -> isChange ca.action) preview.competenceActions
  where
    isChange (Create _) = True
    isChange (Update _ _) = True
    isChange (NoChange _) = False

-- | Apply all grid import previews
applyPreviews :: SyncContext -> Document -> [GridImportPreview] -> IO ()
applyPreviews r doc previews = mapM_ (applyGridPreview r doc) previews

-- | Apply a single grid import preview
-- Note: Currently only handles Create actions. Update is not yet implemented.
applyGridPreview :: SyncContext -> Document -> GridImportPreview -> IO ()
applyGridPreview r _doc preview = do
  -- Handle grid action
  gridId <- case preview.gridAction of
    Create g -> do
      newId <- nextId r
      let newGrid =
            CompetenceGrid
              { id = newId
              , order = orderMax
              , title = g.title
              , description = g.description
              }
      modifySyncDocument r (Cmd.Competences $ Cmd.OnCompetenceGrids $ Cmd.Create newGrid)
      pure newId
    Update _old new -> pure new.id -- Updates not yet implemented
    NoChange g -> pure g.id

  -- Handle competence actions
  mapM_ (applyCompetenceAction r gridId) preview.competenceActions

-- | Apply a single competence import action
-- Note: Currently only handles Create actions. Update is not yet implemented.
applyCompetenceAction :: SyncContext -> Id CompetenceGrid -> CompetenceImportAction -> IO ()
applyCompetenceAction r gridId ca = case ca.action of
  Create c -> do
    newId <- nextId r
    let newComp =
          Competence
            { id = newId
            , competenceGridId = gridId
            , order = orderMax
            , description = c.description
            , levels = c.levels
            }
    modifySyncDocument r (Cmd.Competences $ Cmd.OnCompetences $ Cmd.Create newComp)
  Update _ _ -> pure () -- Updates not yet implemented
  NoChange _ -> pure ()
