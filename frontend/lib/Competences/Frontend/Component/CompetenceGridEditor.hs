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
  , CompetenceId
  , Document (..)
  , Level (..)
  , Lock (..)
  , Order
  , emptyDocument
  , levels
  , orderMax
  , ordered, CompetenceIxs
  )
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocument (..)
  , SyncDocumentRef
  , modifySyncDocument
  , nextId
  , readSyncDocument
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Tuple (Solo (..))
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((%), (&), (.~), (^.), (?~))
import Optics.Core qualified as O
import qualified Competences.Frontend.Component.Editor.FlowView as TE
import qualified Competences.Frontend.Component.Editor.TableView as TE
import Competences.Document.Order (orderPosition)
import Competences.Frontend.Component.Editor.Types (translateReorder')
import Competences.Frontend.Component.Static (StaticComponent, StaticView, staticComponent)

newtype Model = Model
  { competences :: Ix.IxSet CompetenceIxs Competence
  }
  deriving (Eq, Generic, Show)

data Action
  = CreateNewCompetence
  deriving (Eq, Generic, Show)

competenceGridEditorComponent :: SyncDocumentRef -> StaticComponent p Action
competenceGridEditorComponent r = staticComponent update view
  where
    update CreateNewCompetence = M.io_ $ do
      d <- readSyncDocument r
      competenceId <- nextId r
      let competence =
            Competence
              { id = competenceId
              , competenceGridId = d ^. (#localDocument % #competenceGrid % #id)
              , order = orderMax
              , description = ""
              , levelDescriptions = Map.empty
              }
      modifySyncDocument r (OnCompetences $ Create competence)
      modifySyncDocument r (OnCompetences $ Modify competenceId Lock)

    view :: StaticView Action
    view =
      let title = M.div_ [] M.+> competenceGridTitleEditor r
          description = M.div_ [] M.+> competenceGridDescriptionEditor r
          competenceEditable =
            TE.editable
              (\d -> map
                    (\c -> (c, (d ^. #locks) Map.!? CompetenceLock c.id))
                    (Ix.toAscList (Proxy @Order) $ d ^. #competences))
              & (#modify ?~ (\c m -> OnCompetences (Modify c.id m)))
              & (#delete ?~ (\c -> OnCompetences (Delete c.id)))
              & (#reorder ?~ (\d c a -> do
                                p <- orderPosition d.competences c.id
                                pure $ ReorderCompetence p (translateReorder' (.id) a)))

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
          competences = M.div_ [] M.+> TE.editorComponent competencesEditor r

          newCompetenceButton = V.viewButton $ V.iconLabelButton' V.IcnAdd C.LblAddNewCompetence CreateNewCompetence
       in V.viewFlow
            ( V.vFlow
                & (#expandOrthogonal .~ V.Expand V.Start)
                & (#gap .~ V.SmallSpace)
            )
            [ title
            , description
            , competences
            , newCompetenceButton
            ]
    competenceGridTitleEditor = TE.editorComponent editor
      where
        editable =
          TE.editable
            (\d -> MkSolo (d ^. #competenceGrid % #title, (d ^. #locks) Map.!? CompetenceGridTitleLock))
            & (#modify ?~ const ModifyCompetenceGridTitle)
        editor = TE.editor TE.editorFlowView editable `TE.addField` TE.textEditorField (O.castOptic TE.msIso)
    competenceGridDescriptionEditor = TE.editorComponent editor
      where
        editable =
          TE.editable
            (\d -> MkSolo (d ^. #competenceGrid % #description, (d ^. #locks) Map.!? CompetenceGridDescriptionLock))
            & (#modify ?~ const ModifyCompetenceGridDescription)
        editor = TE.editor TE.editorFlowView editable `TE.addField` TE.textEditorField (O.castOptic TE.msIso)
