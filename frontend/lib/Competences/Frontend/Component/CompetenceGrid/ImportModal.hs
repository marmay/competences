{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Frontend.Component.CompetenceGrid.ImportModal
-- Description : Modal component for importing competence grids
--
-- Provides a modal dialog for importing competence grids from a markdown-like
-- format. Shows a preview of changes before applying.
module Competences.Frontend.Component.CompetenceGrid.ImportModal
  ( competenceGridImportModalComponent
  , Action (..)
  )
where

import Competences.Command (CompetencePatch (..), LevelInfoPatch (..), ModifyCommand (..))
import Competences.Command qualified as Cmd
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..))
import Competences.Document.Competence (Competence (..), Level (..), LevelInfo (..))
import Competences.Document.CompetenceGrid (CompetenceGrid (..))
import Competences.Document.Id (Id (..))
import Competences.Document.Order (orderMax)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext (..)
  , closeModal
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.View.Badge (BadgeVariant (..), badge)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Modal qualified as Modal
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
  | CloseModal
  deriving (Eq, Show)

-- ============================================================================
-- Component
-- ============================================================================

competenceGridImportModalComponent :: SyncContext -> M.Component p Model Action
competenceGridImportModalComponent r =
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
        , mesoPlans = Ix.empty
        , lessons = Ix.empty
        , participationRecords = Ix.empty
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
        Right previews -> do
          M.io_ $ do
            applyPreviews r m.document previews
            closeModal r.modalManager
        Left _ -> pure ()

    update CloseModal =
      M.io_ $ closeModal r.modalManager

    view :: Model -> M.View Model Action
    view m =
      -- Note: No modalHost wrapper - the parent ModalHost component provides the backdrop
      M.div_
        [class_ "bg-popover text-popover-foreground rounded-xl shadow-lg w-[80vw] h-[80vh] max-w-[80vw] flex flex-col"]
            [ Modal.modalHeader (C.translate' C.LblImportCompetenceGrids) CloseModal
            , -- Content
              M.div_
                [class_ "flex-1 min-h-0 flex gap-4 p-4 overflow-hidden"]
                [ -- Left: Input area
                  M.div_
                    [class_ "flex flex-col gap-2 min-h-0 flex-1 w-1/2"]
                    [ Typography.h3 "Eingabe"
                    , M.textarea_
                        [ class_ "flex-1 min-h-0 w-full p-3 font-mono text-sm border border-input rounded-md bg-background resize-none"
                        , MP.placeholder_ "# Rastername\n\n## Kompetenzbeschreibung\n- Wesentlich: ...\n- Mittelstufe: ...\n- Fortgeschritten: ..."
                        , MP.value_ (M.ms m.inputText)
                        , M.onInput (SetInputText . M.fromMisoString)
                        ]
                        []
                    ]
                , -- Right: Preview area
                  M.div_
                    [class_ "flex flex-col gap-2 min-h-0 flex-1 w-1/2"]
                    [ Typography.h3 "Vorschau"
                    , M.div_
                        [class_ "flex-1 min-h-0 overflow-y-auto border border-border rounded-md p-3 bg-muted/30"]
                        [previewView m]
                    ]
                ]
            , Modal.modalFooter
                [ Button.buttonSecondary (C.translate' C.LblCancel)
                    & Button.withClick CloseModal
                    & Button.renderButton
                , Button.buttonPrimary "Vorschau"
                    & Button.withClick ParseInput
                    & Button.renderButton
                , case m.parseResult of
                    Right previews
                      | not (null previews) && any hasChanges previews ->
                          Button.buttonPrimary (C.translate' C.LblApply)
                            & Button.withIcon IcnApply
                            & Button.withClick ApplyImport
                            & Button.renderButton
                    _ -> M.text ""
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
  M.div_
    [class_ "border border-border rounded-md p-3"]
    [ M.div_
        [class_ "flex items-center gap-2 mb-2"]
        [ M.span_ [class_ "font-semibold"] [M.text $ M.ms $ gridTitle preview.gridAction]
        , actionBadge preview.gridAction
        ]
    , M.div_
        [class_ "flex flex-col gap-2"]
        ( map previewCompetenceView preview.competenceActions
            ++ map previewDeletedCompetence preview.competencesToDelete
        )
    ]

-- | Preview for a competence that will be deleted
previewDeletedCompetence :: Competence -> M.View Model Action
previewDeletedCompetence c =
  M.div_
    [class_ "pl-4 border-l-2 border-destructive/50"]
    [ M.div_
        [class_ "flex items-center gap-2"]
        [ M.span_
            [class_ "font-medium text-muted-foreground line-through"]
            [M.text $ M.ms c.description]
        , badge BadgeDestructive "Löschen"
        ]
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
            [class_ "font-medium text-sm"]
            [M.text $ M.ms ca.parsedCompetence.description]
        , actionBadge ca.action
        ]
    , M.div_
        [class_ "text-xs text-muted-foreground mt-1"]
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
          , M.text $ M.ms $ T.take 40 desc <> if T.length desc > 40 then "..." else ""
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
    || not (null preview.competencesToDelete)
  where
    isChange (Create _) = True
    isChange (Update _ _) = True
    isChange (NoChange _) = False

-- | Apply all grid import previews
applyPreviews :: SyncContext -> Document -> [GridImportPreview] -> IO ()
applyPreviews r doc previews = mapM_ (applyGridPreview r doc) previews

-- | Apply a single grid import preview
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
    Update _old new -> pure new.id -- Grid updates not yet implemented
    NoChange g -> pure g.id

  -- Handle competence actions (create/update)
  mapM_ (applyCompetenceAction r gridId) preview.competenceActions

  -- Delete competences not in the import
  mapM_ (deleteCompetence r) preview.competencesToDelete

-- | Delete a competence
deleteCompetence :: SyncContext -> Competence -> IO ()
deleteCompetence r c =
  modifySyncDocument r (Cmd.Competences $ Cmd.OnCompetences $ Cmd.Delete c.id)

-- | Apply a single competence import action
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
  Update old new -> do
    modifySyncDocument r (Cmd.Competences $ Cmd.OnCompetences $ Cmd.Modify old.id Lock)
    let patch = buildCompetencePatch old new
    modifySyncDocument r (Cmd.Competences $ Cmd.OnCompetences $ Cmd.Modify old.id (Release patch))
  NoChange _ -> pure ()

-- | Build a CompetencePatch from old and new Competence values
buildCompetencePatch :: Competence -> Competence -> CompetencePatch
buildCompetencePatch old new =
  CompetencePatch
    { description =
        if old.description == new.description
          then Nothing
          else Just (old.description, new.description)
    , levels = buildLevelPatches old.levels new.levels
    }

-- | Build level patches for all levels that have changes
buildLevelPatches :: Map.Map Level LevelInfo -> Map.Map Level LevelInfo -> Map.Map Level LevelInfoPatch
buildLevelPatches oldLevels newLevels =
  Map.mapMaybe id $
    Map.unionWith mergeLevelPatch
      (Map.mapWithKey (buildLevelPatch oldLevels) newLevels)
      (Map.mapWithKey (buildDeletedLevelPatch newLevels) oldLevels)
  where
    -- Build patch for level present in new (may need update)
    buildLevelPatch :: Map.Map Level LevelInfo -> Level -> LevelInfo -> Maybe LevelInfoPatch
    buildLevelPatch olds lvl newInfo =
      let oldInfo = Map.findWithDefault emptyLevelInfo lvl olds
          descChange =
            if oldInfo.description == newInfo.description
              then Nothing
              else Just (oldInfo.description, newInfo.description)
          lockChange =
            if oldInfo.locked == newInfo.locked
              then Nothing
              else Just (oldInfo.locked, newInfo.locked)
       in if descChange == Nothing && lockChange == Nothing
            then Nothing
            else Just LevelInfoPatch {description = descChange, locked = lockChange}

    -- Handle levels that exist in old but not in new (clear them)
    buildDeletedLevelPatch :: Map.Map Level LevelInfo -> Level -> LevelInfo -> Maybe LevelInfoPatch
    buildDeletedLevelPatch news lvl oldInfo =
      if Map.member lvl news
        then Nothing -- Will be handled by buildLevelPatch
        else
          Just
            LevelInfoPatch
              { description = Just (oldInfo.description, T.empty)
              , locked = Just (oldInfo.locked, False)
              }

    -- Merge patches (prefer non-Nothing)
    mergeLevelPatch :: Maybe LevelInfoPatch -> Maybe LevelInfoPatch -> Maybe LevelInfoPatch
    mergeLevelPatch (Just p) _ = Just p
    mergeLevelPatch _ p = p

    emptyLevelInfo = LevelInfo {description = T.empty, locked = False}
