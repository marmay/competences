module Competences.Frontend.Component.CompetenceGridEditor
  ( Model
  , Action
  , competenceGridEditorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..))
import Competences.Document
  ( Competence (..)
  , CompetenceGrid (..)
  , CompetenceId
  , Document (..)
  , Level (..)
  , Lock (..)
  , emptyDocument
  , levels
  , ordered
  )
import Competences.Document.Order (Reorder, orderPosition)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , modifySyncDocument
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core ((%), (&), (.~), (^.))
import Optics.Core qualified as O
import Data.Tuple (Solo(..))
import qualified Data.Map as Map

data Model = Model
  { document :: !Document
  , reorderFrom :: !(C.ReorderModel Competence)
  }
  deriving (Eq, Generic, Show)

data Action
  = UpdateDocument !DocumentChange
  | IssueCommand !Command
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
    update (IssueCommand cmd) = M.io_ $ modifySyncDocument r cmd
    view :: Model -> M.View m Action
    view m =
      let title = M.div_ [] M.+> competenceGridTitleEditor r
          description = M.div_ [] M.+> competenceGridDescriptionEditor r
          competences =
            V.viewTable $
              V.Table
                { columns =
                    [ DescriptionColumn
                    ]
                      <> map LevelDescriptionColumn levels
                      <> [ActionColumn]
                , rows = ordered m.document.competences
                , columnSpec = \case
                    ActionColumn -> V.SingleActionColumn
                    _ -> V.AutoSizedColumn
                , columnHeader = \c -> fromMaybe "" $ case c of
                    DescriptionColumn -> Just $ C.translate' C.LblCompetenceDescription
                    LevelDescriptionColumn level -> Just $ C.translate' $ C.LblCompetenceLevelDescription level
                    _ -> Nothing
                , cellContents = \competence -> \case
                    MoveColumn -> M.div_ [] []
                    DescriptionColumn -> M.div_ [] []
                    LevelDescriptionColumn level -> M.div_ [] []
                    ActionColumn -> V.viewButtons V.hButtons [V.deleteButton (IssueCommand (OnCompetences (Delete competence.id)))]
                }
       in V.viewFlow
            ( V.vFlow
                & (#expandOrthogonal .~ V.Expand V.Start)
                & (#gap .~ V.SmallSpace)
            )
            [ title
            , description
            , competences
            ]
    competenceGridTitleEditor = TE.editorComponent editor
      where
        editable =
          TE.Editable
          { get = \d -> MkSolo (d ^. #competenceGrid % #title, (d ^. #locks) Map.!? CompetenceGridTitleLock)
          , modify = const ModifyCompetenceGridTitle
          }
        editor = TE.flowEditor editable `TE.addField` TE.textEditorField TE.msIso
    competenceGridDescriptionEditor = TE.editorComponent editor
      where
        editable =
          TE.Editable
          { get = \d -> MkSolo (d ^. #competenceGrid % #description, (d ^. #locks) Map.!? CompetenceGridDescriptionLock)
          , modify = const ModifyCompetenceGridDescription
          }
        editor = TE.flowEditor editable `TE.addField` TE.textEditorField TE.msIso
