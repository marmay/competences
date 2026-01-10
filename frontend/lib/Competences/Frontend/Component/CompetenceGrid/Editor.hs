module Competences.Frontend.Component.CompetenceGrid.Editor
  ( editorDetailView
  )
where

import Competences.Command (Command (..), CompetenceGridPatch (..), CompetencePatch (..), CompetencesCommand (..), EntityCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceGrid (..)
  , Document (..)
  , Level (..)
  , Lock (..)
  , Order
  , orderMax
  )
import Competences.Document.Order (orderPosition)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.FormView qualified as TE
import Competences.Frontend.Component.Editor.TableView qualified as TE
import Competences.Frontend.Component.Editor.Types (translateReorder')
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncDocument
  ( SyncContext
  , modifySyncDocument
  , nextId
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon (Icon (..))
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
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
              , levelDescriptions = Map.empty
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
                           , TE.textEditorField (#levelDescriptions % O.at BasicLevel % O.non T.empty) (#levelDescriptions % O.at BasicLevel % O.non Nothing)
                           )
        `TE.addNamedField` ( C.translate' (C.LblCompetenceLevelDescription IntermediateLevel)
                           , TE.textEditorField (#levelDescriptions % O.at IntermediateLevel % O.non T.empty) (#levelDescriptions % O.at IntermediateLevel % O.non Nothing)
                           )
        `TE.addNamedField` ( C.translate' (C.LblCompetenceLevelDescription AdvancedLevel)
                           , TE.textEditorField (#levelDescriptions % O.at AdvancedLevel % O.non T.empty) (#levelDescriptions % O.at AdvancedLevel % O.non Nothing)
                           )
