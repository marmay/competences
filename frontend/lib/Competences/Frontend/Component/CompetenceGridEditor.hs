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
  , ordered
  )
import Competences.Document.Order (Reorder, orderPosition)
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
import Control.Monad (forM_)
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import Data.Tuple (Solo (..))
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((%), (&), (.~), (^.))
import Optics.Core qualified as O

data Model = Model
  { document :: !Document
  , reorderFrom :: !(C.ReorderModel Competence)
  }
  deriving (Eq, Generic, Show)

data Action
  = UpdateDocument !DocumentChange
  | IssueCommands ![Command]
  | CreateNewCompetence
  deriving (Eq, Generic, Show)

data CompetenceGridColumn
  = MoveColumn
  | DescriptionColumn
  | LevelDescriptionColumn !Level
  | ActionColumn
  deriving (Eq, Ord, Show)

competenceGridEditorComponent :: SyncDocumentRef -> M.Component p Model Action
competenceGridEditorComponent r =
  (M.component model update view) {M.subs = [subscribeDocument r UpdateDocument]}
  where
    model =
      Model
        { document = emptyDocument
        , reorderFrom = C.initialReorderModel
        }

    update (UpdateDocument (DocumentChange newDocument _)) = do
      M.modify $ \s ->
        s
          & (#document .~ newDocument)
    update (IssueCommands cmds) = M.io_ $ forM_ cmds $ modifySyncDocument r
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
    view :: Model -> M.View m Action
    view _ =
      let title = M.div_ [] M.+> competenceGridTitleEditor r
          description = M.div_ [] M.+> competenceGridDescriptionEditor r
          competenceEditable =
            TE.Editable
              { get = \d ->
                  map
                    (\c -> (c, (d ^. #locks) Map.!? CompetenceLock c.id))
                    (Ix.toAscList (Proxy @Order) $ d ^. #competences)
              , modify = \c m -> OnCompetences (Modify c.id m)
              }

          competencesEditor =
            TE.editor
              (TE.withDeleteAction (\c -> OnCompetences $ Delete c.id) TE.editorTableRowView')
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
          TE.Editable
            { get = \d -> MkSolo (d ^. #competenceGrid % #title, (d ^. #locks) Map.!? CompetenceGridTitleLock)
            , modify = const ModifyCompetenceGridTitle
            }
        editor = TE.flowEditor editable `TE.addField` TE.textEditorField (O.castOptic TE.msIso)
    competenceGridDescriptionEditor = TE.editorComponent editor
      where
        editable =
          TE.Editable
            { get = \d -> MkSolo (d ^. #competenceGrid % #description, (d ^. #locks) Map.!? CompetenceGridDescriptionLock)
            , modify = const ModifyCompetenceGridDescription
            }
        editor = TE.flowEditor editable `TE.addField` TE.textEditorField (O.castOptic TE.msIso)
