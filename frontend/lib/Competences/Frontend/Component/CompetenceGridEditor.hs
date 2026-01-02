module Competences.Frontend.Component.CompetenceGridEditor
  ( Model
  , Action
  , competenceGridEditorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), ModifyCommand (..), CompetencesCommand (..), CompetenceGridPatch (..), CompetencePatch (..))
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
import Competences.Frontend.Component.Selector.CompetenceGridSelector
  ( CompetenceGridSelectorStyle (..)
  , competenceGridSelectorComponent
  )
import Competences.Frontend.SyncDocument
  ( SyncDocumentRef
  , modifySyncDocument
  , nextId
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Typography qualified as Typography
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core ((%), (&), (.~), (?~), (^.))
import Optics.Core qualified as O

data Model = Model
  { competenceGrid :: !(Maybe CompetenceGrid)
  }
  deriving (Eq, Generic, Show)

data Action
  = CreateNewCompetence
  deriving (Eq, Generic, Show)

competenceGridEditorComponent :: SyncDocumentRef -> M.Component p Model Action
competenceGridEditorComponent r = M.component emptyModel update view
  where
    emptyModel = Model Nothing

    update CreateNewCompetence = do
      m <- M.get
      case m.competenceGrid of
        Just competenceGrid -> M.io_ $ do
          competenceId <- nextId r
          let competence =
                Competence
                  { id = competenceId
                  , competenceGridId = competenceGrid.id
                  , order = orderMax
                  , description = ""
                  , levelDescriptions = Map.empty
                  }
          modifySyncDocument r (Competences $ OnCompetences $ Create competence)
          modifySyncDocument r (Competences $ OnCompetences $ Modify competenceId Lock)
        Nothing -> pure ()

    view model =
      V.viewFlow
        (V.hFlow & (#expandDirection .~ V.Expand V.Start) & (#expandOrthogonal .~ V.Expand V.Start))
        [ V.component
            "competence-grid-viewer-selection"
            (competenceGridSelectorComponent r CompetenceGridSelectorViewAndCreateStyle #competenceGrid)
        , case model.competenceGrid of
            Just competenceGrid -> view' competenceGrid
            Nothing -> Typography.muted "Bitte wählen Sie einen Kompetenzraster aus"
        ]

    view' competenceGrid =
      let competenceGridEditable =
            TE.editable
              ( \d -> do
                  competenceGrid' <- Ix.getOne $ (d ^. #competenceGrids) Ix.@= competenceGrid.id
                  pure (competenceGrid', (d ^. #locks) Map.!? CompetenceGridLock competenceGrid'.id)
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
                    (Ix.toAscList (Proxy @Order) ((d ^. #competences) Ix.@= competenceGrid.id))
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
       in V.viewFlow
            ( V.vFlow
                & (#expandOrthogonal .~ V.Expand V.Start)
                & (#gap .~ V.SmallSpace)
            )
            [ V.component
                ("competence-grid-editor-competence-grid-" <> M.ms (show competenceGrid.id))
                (TE.editorComponent competenceGridEditor r)
            , V.component
                ("competence-grid-editor-competences-" <> M.ms (show competenceGrid.id))
                (TE.editorComponent competencesEditor r)
            , Button.buttonPrimary (C.translate' C.LblAddNewCompetence)
                & Button.withIcon IcnAdd
                & Button.withClick CreateNewCompetence
                & Button.renderButton
            ]
