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
  , orderMax
  )
import Competences.Document.Order (orderPosition)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.EditorField (EditorField (..))
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Editor.TableView qualified as TE
import Competences.Frontend.Component.Editor.Types (Action (UpdatePatch), Model (..), translateReorder')
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncDocument
  ( SyncContext
  , modifySyncDocument
  , nextId
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon (Icon (..), icon)
import Competences.Frontend.View.Tailwind (class_)
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

import Competences.Frontend.Component.CompetenceGrid.Types (CompetenceGridMode)

-- ============================================================================
-- EDIT MODE DETAIL
-- ============================================================================

-- | Action for the editor detail component
data EditorAction = CreateNewCompetence
  deriving (Eq, Generic, Show)

-- | View for the editor detail - allows editing grid and competences
editorDetailView
  :: SyncContext
  -> CompetenceGrid
  -> M.View (SD.Model CompetenceGrid CompetenceGridMode) (SD.Action CompetenceGridMode)
editorDetailView r grid =
  V.component
    ("competence-grid-editor-" <> M.ms (show grid.id))
    (editorComponent r grid)

editorComponent :: SyncContext -> CompetenceGrid -> M.Component p () EditorAction
editorComponent r grid =
  M.component () update view
  where
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

    view _ =
      V.viewFlow
        ( V.vFlow
            & (#expandDirection .~ V.Expand V.Start)
            & (#expandOrthogonal .~ V.Expand V.Center)
            & (#gap .~ V.SmallSpace)
        )
        [ V.component
            ("competence-grid-editor-grid-" <> M.ms (show grid.id))
            (TE.editorComponent competenceGridEditor r)
        , V.component
            ("competence-grid-editor-competences-" <> M.ms (show grid.id))
            (TE.editorComponent competencesEditor r)
        , Button.buttonPrimary (C.translate' C.LblAddNewCompetence)
            & Button.withIcon IcnAdd
            & Button.withClick CreateNewCompetence
            & Button.renderButton
        ]

    competenceGridEditable =
      TE.editable
        ( \d -> do
            grid' <- Ix.getOne $ (d ^. #competenceGrids) Ix.@= grid.id
            pure (grid', (d ^. #locks) Map.!? CompetenceGridLock grid'.id)
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
              (\c -> (c, (d ^. #locks) Map.!? CompetenceLock c.id))
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
                           , levelDescriptionWithLockField BasicLevel
                           )
        `TE.addNamedField` ( C.translate' (C.LblCompetenceLevelDescription IntermediateLevel)
                           , levelDescriptionWithLockField IntermediateLevel
                           )
        `TE.addNamedField` ( C.translate' (C.LblCompetenceLevelDescription AdvancedLevel)
                           , levelDescriptionWithLockField AdvancedLevel
                           )

-- | Combined editor field for level description with lock toggle
-- Shows text input with a lock button next to it
-- Lock button is only enabled when description is non-empty
levelDescriptionWithLockField :: Level -> EditorField Competence CompetencePatch f
levelDescriptionWithLockField lvl =
  EditorField
    { viewer = levelDescriptionWithLockViewer lvl
    , editor = levelDescriptionWithLockEditor lvl
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

-- | Viewer for level description with lock indicator
levelDescriptionWithLockViewer :: Level -> Competence -> M.View (Model Competence CompetencePatch f) (Action Competence CompetencePatch)
levelDescriptionWithLockViewer lvl c =
  let info = Map.findWithDefault (LevelInfo T.empty False) lvl c.levels
   in MH.div_ [class_ "flex items-center gap-2"]
        [ MH.span_ [class_ "flex-1"] [V.text_ (M.ms info.description)]
        , if info.locked
            then MH.span_ [class_ "text-stone-400"] [icon [MP.width_ "14", MP.height_ "14"] IcnLock]
            else V.empty
        ]

-- | Editor for level description with lock toggle
levelDescriptionWithLockEditor
  :: Level
  -> Bool
  -> Competence
  -> CompetencePatch
  -> M.View (Model Competence CompetencePatch f) (Action Competence CompetencePatch)
levelDescriptionWithLockEditor lvl _refocusTarget original patch =
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
   in MH.div_ [class_ "flex items-center gap-1"]
        [ MH.input_
            [ class_ "flex-1 px-2 py-1 border border-stone-300 rounded text-sm"
            , MH.onChange updateDesc
            , MP.value_ (M.ms currentInfo.description)
            ]
        , MH.button_
            ( [ class_ $
                  "w-7 h-7 flex items-center justify-center rounded border transition-colors "
                    <> if not hasDescription
                         then "bg-stone-50 border-stone-200 text-stone-300 cursor-not-allowed"
                         else if currentInfo.locked
                           then "bg-stone-200 border-stone-400 text-stone-700 hover:bg-stone-300 cursor-pointer"
                           else "bg-white border-stone-200 text-stone-400 hover:bg-stone-50 cursor-pointer"
              , MP.type_ "button"
              , MH.onClick toggleLock
              , MP.title_ $
                  if not hasDescription
                    then "Add description to enable locking"
                    else if currentInfo.locked
                      then "Unlock this level"
                      else "Lock this level"
              ]
                <> [MP.disabled_ | not hasDescription]
            )
            [ icon [MP.width_ "14", MP.height_ "14"] (if currentInfo.locked then IcnLock else IcnLockOpen)
            ]
        ]
