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
  , openCompetenceGridImportModal
  , Action
  )
where

import Competences.Command (CompetencePatch (..), LevelInfoPatch (..), ModifyCommand (..))
import Competences.Command qualified as Cmd
import Competences.Document (Document (..))
import Competences.Document.Competence (Competence (..), Level (..), LevelInfo (..))
import Competences.Document.CompetenceGrid (CompetenceGrid (..))
import Competences.Document.Id (Id (..))
import Competences.Document.Order (orderMax)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.ImportModal qualified as IM
import Competences.Frontend.SyncContext
  ( SyncContext (..)
  , modifySyncDocument
  , nextId
  )
import Competences.Frontend.SyncContext.WindowManager (ModalConfig (..), ModalId (..), ModalHeight (..), ModalWidth (..), WindowChrome (..), WindowMode, closeWindow, openFramedModalWith)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Badge qualified as Badge
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Tailwind (class_)
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
import Miso qualified as M
import Miso.Html qualified as M

-- ============================================================================
-- Types (re-exports from generic module)
-- ============================================================================

type Action = IM.Action

-- ============================================================================
-- Component
-- ============================================================================

-- | Open the competence grid import modal as a framed modal.
openCompetenceGridImportModal :: SyncContext -> IO ()
openCompetenceGridImportModal r =
  let cfg = ModalConfig (WindowChrome (C.translate' C.LblImportCompetenceGrids) Icon.IcnImport) (ModalId "import-competence-grids") ModalWide ModalFull Nothing
   in openFramedModalWith r.windowManager cfg (competenceGridImportModalComponent r)

competenceGridImportModalComponent :: SyncContext -> WindowMode -> M.Component p (IM.Model GridImportPreview) Action
competenceGridImportModalComponent = IM.importModalComponent gridImportConfig

gridImportConfig :: IM.ImportModalConfig GridImportPreview
gridImportConfig =
  IM.ImportModalConfig
    { parse = \doc input -> case parseGridImport input of
        Left err -> Left err
        Right parsed -> Right $ matchGridImport doc parsed
    , renderItem = previewGridView
    , hasChanges = gridHasChanges
    , apply = applyGridImport
    , placeholder = "# Rastername\n\n## Kompetenzbeschreibung\n- Wesentlich: ...\n- Mittelstufe: ...\n- Fortgeschritten: ..."
    }

-- ============================================================================
-- Preview View
-- ============================================================================

previewGridView :: GridImportPreview -> M.View (IM.Model GridImportPreview) IM.Action
previewGridView preview =
  M.div_
    [class_ "border border-border rounded-md p-3"]
    [ M.div_
        [class_ "mb-2"]
        [ Layout.hFlow
            (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
            [ M.span_ [class_ "font-semibold"] [M.text $ M.ms $ gridTitle preview.gridAction]
            , IM.actionBadge preview.gridAction
            ]
        ]
    , Layout.vFlow Layout.gapS
        ( map previewCompetenceView preview.competenceActions
            ++ map previewDeletedCompetence preview.competencesToDelete
        )
    ]

previewDeletedCompetence :: Competence -> M.View (IM.Model GridImportPreview) IM.Action
previewDeletedCompetence c =
  M.div_
    [class_ "pl-4 border-l-2 border-destructive/50"]
    [ Layout.hFlow
        (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
        [ M.span_
            [class_ "font-medium text-muted-foreground line-through"]
            [M.text $ M.ms c.description]
        , Badge.destructive (Badge.badgeText "Löschen")
        ]
    ]

gridTitle :: ImportAction CompetenceGrid -> Text
gridTitle (Create g) = g.title
gridTitle (Update _ g) = g.title
gridTitle (NoChange g) = g.title

previewCompetenceView :: CompetenceImportAction -> M.View (IM.Model GridImportPreview) IM.Action
previewCompetenceView ca =
  M.div_
    [class_ "pl-4 border-l-2 border-border"]
    [ Layout.hFlow
        (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
        [ M.span_
            [class_ "font-medium text-sm"]
            [M.text $ M.ms ca.parsedCompetence.description]
        , IM.actionBadge ca.action
        ]
    , M.div_
        [class_ "text-xs text-muted-foreground mt-1"]
        (levelPreview ca.parsedCompetence.levels)
    ]

levelPreview :: Map.Map Level Text -> [M.View (IM.Model GridImportPreview) IM.Action]
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

-- ============================================================================
-- Change Detection
-- ============================================================================

gridHasChanges :: GridImportPreview -> Bool
gridHasChanges preview =
  isChange preview.gridAction
    || any (\ca -> isChange ca.action) preview.competenceActions
    || not (null preview.competencesToDelete)

isChange :: ImportAction a -> Bool
isChange (Create _) = True
isChange (Update _ _) = True
isChange (NoChange _) = False

-- ============================================================================
-- Apply Import
-- ============================================================================

applyGridImport :: SyncContext -> WindowMode -> Document -> [GridImportPreview] -> IO ()
applyGridImport r wm doc previews = do
  mapM_ (applyGridPreview r doc) previews
  closeWindow wm

applyGridPreview :: SyncContext -> Document -> GridImportPreview -> IO ()
applyGridPreview r _doc preview = do
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
    Update _old new -> pure new.id
    NoChange g -> pure g.id

  mapM_ (applyCompetenceAction r gridId) preview.competenceActions
  mapM_ (deleteCompetence r) preview.competencesToDelete

deleteCompetence :: SyncContext -> Competence -> IO ()
deleteCompetence r c =
  modifySyncDocument r (Cmd.Competences $ Cmd.OnCompetences $ Cmd.Delete c.id)

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

buildCompetencePatch :: Competence -> Competence -> CompetencePatch
buildCompetencePatch old new =
  CompetencePatch
    { description =
        if old.description == new.description
          then Nothing
          else Just (old.description, new.description)
    , levels = buildLevelPatches old.levels new.levels
    }

buildLevelPatches :: Map.Map Level LevelInfo -> Map.Map Level LevelInfo -> Map.Map Level LevelInfoPatch
buildLevelPatches oldLevels newLevels =
  Map.mapMaybe id $
    Map.unionWith mergeLevelPatch
      (Map.mapWithKey (buildLevelPatch oldLevels) newLevels)
      (Map.mapWithKey (buildDeletedLevelPatch newLevels) oldLevels)
  where
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

    buildDeletedLevelPatch :: Map.Map Level LevelInfo -> Level -> LevelInfo -> Maybe LevelInfoPatch
    buildDeletedLevelPatch news lvl oldInfo =
      if Map.member lvl news
        then Nothing
        else
          Just
            LevelInfoPatch
              { description = Just (oldInfo.description, T.empty)
              , locked = Just (oldInfo.locked, False)
              }

    mergeLevelPatch :: Maybe LevelInfoPatch -> Maybe LevelInfoPatch -> Maybe LevelInfoPatch
    mergeLevelPatch (Just p) _ = Just p
    mergeLevelPatch _ p = p

    emptyLevelInfo = LevelInfo {description = T.empty, locked = False}
