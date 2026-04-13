module Competences.Frontend.Component.CompetenceGrid.Editor
  ( editorDetailView
  )
where

import Competences.Command (Command (..), CompetenceGridPatch (..), CompetencePatch (..), LevelInfoPatch (..), CompetencesCommand (..), EntityCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceGrid (..)
  , Document (..)
  , Level (..)
  , LevelInfo (..)
  , Lock (..)
  , Order
  , lockOwner
  , orderMax
  )
import Competences.Document.Order (orderPosition)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.CompetenceGrid.Types (CompetenceGridMode)
import Competences.Frontend.Component.CompetenceLevelExampleEditor (openExampleEditor)
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.EditorField (EditorField (..))
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Editor.TableView qualified as TE
import Competences.Frontend.Component.Editor.Types (Action (UpdatePatch), Model (..), translateReorder')
import Competences.Frontend.Component.ExportButton (exportButtonComponent)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext (..)
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.SyncContext.WindowManager (inlineComponent)
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.Text (text_)
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.StatusIcon qualified as StatusIcon
import Competences.Frontend.View.Tailwind (class_)
import Competences.Import.Export (exportCompetenceGrid)
import Data.Default (def)
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP
import Optics.Core ((&), (?~), (^.), (.~), (%))
import Optics.Core qualified as O

-- ============================================================================
-- EDIT MODE DETAIL
-- ============================================================================

-- | Model for the editor detail component
newtype EditorModel = EditorModel
  { document :: Document
  }
  deriving (Eq, Generic, Show)

-- | Action for the editor detail component
data EditorAction
  = CreateNewCompetence
  | DocumentUpdated !DocumentChange
  deriving (Eq, Show)

-- | View for the editor detail - allows editing grid and competences
editorDetailView
  :: SyncContext
  -> CompetenceGrid
  -> M.View (SD.Model CompetenceGrid CompetenceGridMode) (SD.Action CompetenceGridMode)
editorDetailView r grid =
  inlineComponent
    ("competence-grid-editor-" <> M.ms (show grid.id))
    (editorComponent r grid)

editorComponent :: SyncContext -> CompetenceGrid -> M.Component p EditorModel EditorAction
editorComponent r grid =
  (M.component initialModel update view)
    { M.subs = [subscribeDocument r DocumentUpdated]
    }
  where
    initialModel = EditorModel {document = emptyDocument}

    emptyDocument =
      Document
        { competenceGrids = Ix.empty
        , competences = Ix.empty
        , users = Ix.empty
        , evidences = Ix.empty
        , locks = mempty
        , tasks = Ix.empty
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
        , draftAssignments = Ix.empty
        , competenceLevelExamples = Ix.empty
        , layouts = Ix.empty
        }

    update (DocumentUpdated dc) = M.modify $ #document .~ dc.document

    update CreateNewCompetence = M.io_ $ do
      competenceId <- nextId r
      let competence =
            Competence
              { id = competenceId
              , competenceGridId = grid.id
              , order = orderMax
              , description = ""
              , levels = Map.empty
              }
      modifySyncDocument r (Competences $ OnCompetences $ CreateAndLock competence)

    view _m =
      Layout.vFlow
        (Layout.gapS <> Layout.wFull <> Layout.crossCenter)
        [ inlineComponent
            ("competence-grid-editor-grid-" <> M.ms (show grid.id))
            (TE.editorComponent competenceGridEditor r def)
        , inlineComponent
            ("competence-grid-editor-competences-" <> M.ms (show grid.id))
            (TE.editorComponent competencesEditor r def)
        , Layout.hFlow Layout.gapS
            [ Button.primary (Button.button (Icon.IcnAdd, C.LblAddNewCompetence) CreateNewCompetence)
            , inlineComponent
                ("export-btn-" <> M.ms (show grid.id))
                (exportButtonComponent (\m' -> exportCompetenceGrid m'.document grid))
            ]
        ]

    competenceGridEditable =
      TE.editable
        ( \d -> do
            grid' <- Ix.getOne $ (d ^. #competenceGrids) Ix.@= grid.id
            pure (grid', lockOwner (CompetenceGridLock grid'.id) d)
        )
        & (#modify ?~ (\c m -> Competences $ OnCompetenceGrids (Modify c.id m)))
        & (#delete ?~ (\c -> Competences $ OnCompetenceGrids (Delete c.id)))

    competenceGridEditor =
      TE.editor
        ( TE.editorFormView'
            (C.translate' C.LblCompetenceGrid)
            id
        )
        competenceGridEditable
        `TE.addNamedField` ( C.translate' C.LblCompetenceGridTitle
                           , TE.textEditorField #title #title
                           )
        `TE.addNamedField` ( C.translate' C.LblCompetenceGridDescription
                           , TE.textEditorField #description #description
                           )

    competenceEditable =
      TE.editable
        ( \d ->
            map
              (\c -> (c, lockOwner (CompetenceLock c.id) d))
              (Ix.toAscList (Proxy @Order) ((d ^. #competences) Ix.@= grid.id))
        )
        & (#modify ?~ (\c m -> Competences $ OnCompetences (Modify c.id m)))
        & (#delete ?~ (\c -> Competences $ OnCompetences (Delete c.id)))
        & ( #reorder
              ?~ ( \d c a -> do
                     p <- orderPosition d.competences c.id
                     pure $ Competences $ ReorderCompetence p (translateReorder' (.id) a)
                 )
          )

    competencesEditor =
      TE.editor
        TE.editorTableRowView'
        competenceEditable
        `TE.addNamedField` ( C.translate' C.LblCompetenceDescription
                           , TE.textEditorField #description #description
                           )
        `TE.addNamedField` ( C.translate' (C.LblCompetenceLevelDescription BasicLevel)
                           , levelDescriptionWithLockField r BasicLevel
                           )
        `TE.addNamedField` ( C.translate' (C.LblCompetenceLevelDescription IntermediateLevel)
                           , levelDescriptionWithLockField r IntermediateLevel
                           )
        `TE.addNamedField` ( C.translate' (C.LblCompetenceLevelDescription AdvancedLevel)
                           , levelDescriptionWithLockField r AdvancedLevel
                           )

-- | Combined editor field for level description with lock toggle and examples button
-- Shows text input with a lock button next to it
-- Lock button is only enabled when description is non-empty
levelDescriptionWithLockField :: SyncContext -> Level -> EditorField Competence CompetencePatch f
levelDescriptionWithLockField r lvl =
  EditorField
    { viewer = levelDescriptionWithLockViewer r lvl
    , editor = levelDescriptionWithLockEditor r lvl
    }

-- | Get current level info, considering pending patch
currentLevelInfo :: Competence -> CompetencePatch -> Level -> LevelInfo
currentLevelInfo original patch lvl =
  let origInfo = Map.findWithDefault (LevelInfo T.empty False) lvl original.levels
      levelPatch = Map.findWithDefault def lvl patch.levels
      desc = case levelPatch.description of
        Just (_, after) -> after
        Nothing -> origInfo.description
      lck = case levelPatch.locked of
        Just (_, after) -> after
        Nothing -> origInfo.locked
   in LevelInfo desc lck

-- | Viewer for level description with lock indicator and examples button
levelDescriptionWithLockViewer :: SyncContext -> Level -> Competence -> M.View (Model Competence CompetencePatch f) (Action Competence CompetencePatch)
levelDescriptionWithLockViewer r lvl c =
  let info = Map.findWithDefault (LevelInfo T.empty False) lvl c.levels
      btnKey = "examples-btn-" <> M.ms (show c.id) <> "-" <> M.ms (show lvl)
      hasDescription = not (T.null info.description)
   in Layout.hFlow
        (Layout.gapS <> Layout.hFull <> Layout.crossCenter)
        [ MH.span_ [class_ "flex-1"] [text_ (M.ms info.description)]
        , if hasDescription
            then inlineComponent btnKey (examplesButtonComponent r c lvl)
            else Layout.empty
        , if info.locked
            then StatusIcon.lockIcon
            else Layout.empty
        ]

-- | Editor for level description with lock toggle and examples button
levelDescriptionWithLockEditor
  :: SyncContext
  -> Level
  -> Bool
  -> Competence
  -> CompetencePatch
  -> M.View (Model Competence CompetencePatch f) (Action Competence CompetencePatch)
levelDescriptionWithLockEditor r lvl _refocusTarget original patch =
  let currentInfo = currentLevelInfo original patch lvl
      origInfo = Map.findWithDefault (LevelInfo T.empty False) lvl original.levels
      hasDescription = not (T.null currentInfo.description)
      -- Update description patch
      updateDesc v =
        let newDesc = M.fromMisoString v
            levelPatch = Map.findWithDefault def lvl patch.levels
            newLevelPatch = levelPatch & #description ?~ (origInfo.description, newDesc)
            newPatch = patch & #levels % O.at lvl ?~ newLevelPatch
         in UpdatePatch original newPatch
      -- Toggle lock
      toggleLock =
        let newLocked = not currentInfo.locked
            levelPatch = Map.findWithDefault def lvl patch.levels
            newLevelPatch = levelPatch & #locked ?~ (origInfo.locked, newLocked)
            newPatch = patch & #levels % O.at lvl ?~ newLevelPatch
         in UpdatePatch original newPatch
      btnKey = "examples-btn-edit-" <> M.ms (show original.id) <> "-" <> M.ms (show lvl)
   in Layout.hFlow
        (Layout.gapT <> Layout.hFull <> Layout.crossCenter)
        [ MH.input_
            [ class_ "flex-1 px-2 py-1 border border-stone-300 rounded text-sm"
            , MH.onChange updateDesc
            , MP.value_ (M.ms currentInfo.description)
            ]
        , if hasDescription
            then inlineComponent btnKey (examplesButtonComponent r original lvl)
            else Layout.empty
        , Button.toggleSm currentInfo.locked
            ( Button.button
                (if currentInfo.locked then Icon.IcnLock else Icon.IcnLockOpen)
                (hasDescription, toggleLock)
            )
        ]

-- | Inline component that renders an examples button and opens the editor modal on click.
examplesButtonComponent :: SyncContext -> Competence -> Level -> M.Component p () ()
examplesButtonComponent r comp lvl =
  M.component () update' view'
  where
    update' () = M.io_ $ openExampleEditor r comp.description comp.id lvl
    view' () = Button.outlineSm (Button.button Icon.IcnInfo (Just ()))

