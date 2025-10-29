module Competences.Frontend.Component.CompetenceGridEditor
  ( Model
  , Action
  , competenceGridEditorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), ModifyCommand (..))
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
          modifySyncDocument r (OnCompetences $ Create competence)
          modifySyncDocument r (OnCompetences $ Modify competenceId Lock)
        Nothing -> pure ()

    view model =
      V.viewFlow
        (V.hFlow & (#expandDirection .~ V.Expand V.Start) & (#expandOrthogonal .~ V.Expand V.Start))
        [ V.component
            "competence-grid-viewer-selection"
            (competenceGridSelectorComponent r CompetenceGridSelectorViewAndCreateStyle #competenceGrid)
        , case model.competenceGrid of
            Just competenceGrid -> view' competenceGrid
            Nothing -> V.text_ "..."
        ]

    view' competenceGrid =
      let competenceGridEditable =
            TE.editable
              ( \d -> do
                  competenceGrid' <- Ix.getOne $ (d ^. #competenceGrids) Ix.@= competenceGrid.id
                  pure (competenceGrid', (d ^. #locks) Map.!? CompetenceGridLock competenceGrid'.id)
              )
              & (#modify ?~ (\c m -> OnCompetenceGrids (Modify c.id m)))
              & (#delete ?~ (\c -> OnCompetenceGrids (Delete c.id)))
          competenceGridEditor =
            TE.editor
              ( TE.editorFormView'
                  (C.translate' C.LblCompetenceGrid)
                  id
              )
              competenceGridEditable
              `TE.addNamedField` ( C.translate' C.LblCompetenceGridTitle
                                 , TE.textEditorField (#title % TE.msIso)
                                 )
              `TE.addNamedField` ( C.translate' C.LblCompetenceGridDescription
                                 , TE.textEditorField (#description % TE.msIso)
                                 )
          competenceEditable =
            TE.editable
              ( \d ->
                  map
                    (\c -> (c, (d ^. #locks) Map.!? CompetenceLock c.id))
                    (Ix.toAscList (Proxy @Order) $ d ^. #competences)
              )
              & (#modify ?~ (\c m -> OnCompetences (Modify c.id m)))
              & (#delete ?~ (\c -> OnCompetences (Delete c.id)))
              & ( #reorder
                    ?~ ( \d c a -> do
                           p <- orderPosition d.competences c.id
                           pure $ ReorderCompetence p (translateReorder' (.id) a)
                       )
                )

          competencesEditor =
            TE.editor
              TE.editorTableRowView'
              competenceEditable
              `TE.addNamedField` ( C.translate' C.LblCompetenceDescription
                                 , TE.textEditorField (#description % TE.msIso)
                                 )
              `TE.addNamedField` ( C.translate' (C.LblCompetenceLevelDescription BasicLevel)
                                 , TE.textEditorField (#levelDescriptions % O.at BasicLevel % O.non T.empty % TE.msIso)
                                 )
              `TE.addNamedField` ( C.translate' (C.LblCompetenceLevelDescription IntermediateLevel)
                                 , TE.textEditorField (#levelDescriptions % O.at IntermediateLevel % O.non T.empty % TE.msIso)
                                 )
              `TE.addNamedField` ( C.translate' (C.LblCompetenceLevelDescription AdvancedLevel)
                                 , TE.textEditorField (#levelDescriptions % O.at AdvancedLevel % O.non T.empty % TE.msIso)
                                 )
       in V.viewFlow
            ( V.vFlow
                & (#expandOrthogonal .~ V.Expand V.Start)
                & (#gap .~ V.SmallSpace)
            )
            [ V.component "competence-grid-editor-competence-grid" (TE.editorComponent competenceGridEditor r)
            , V.component "competence-grid-editor-competences" (TE.editorComponent competencesEditor r)
            , V.viewButton $ V.iconLabelButton' V.IcnAdd C.LblAddNewCompetence CreateNewCompetence
            ]
