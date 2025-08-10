{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Competences.Frontend.Component.CompetenceGridEditor
  ( Model
  , Action (..)
  , mkModel
  , subscriptions
  , update
  , view
  )
where

import Competences.Command (Command (..))
import Competences.Document
  ( ChangableField (..)
  , Competence (..)
  , CompetenceId
  , Document (..)
  , Level (..)
  , User (..)
  , UserId
  , emptyDocument
  , fieldATraversal
  , ordered
  )
import Competences.Document.Order (Reorder, orderPosition)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentRef
  , modifySyncDocument
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Tailwind qualified as T
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String qualified as M
import Optics.Core ((%~), (&), (.~), (^?))

data Model = Model
  { user :: !User
  , document :: !Document
  , editFields :: !(Map.Map ChangableField M.MisoString)
  , reorderFrom :: !(C.ReorderModel Competence)
  }
  deriving (Eq, Generic, Show)

data Action
  = -- | An internal Action to update the state of edit fields.
    EditField !ChangableField !M.MisoString
  | -- | The host component must trigger this action when the document
    -- has been updated.
    UpdateDocument !DocumentChange
  | -- | The host component must catch those actions and issue the
    -- commands.
    IssueCommand !Command
  | ReorderAction !(C.ReorderAction Competence)
  deriving (Eq, Generic, Show)

mkModel :: User -> Model
mkModel user =
  Model
    { user = user
    , document = emptyDocument
    , editFields = Map.empty
    , reorderFrom = C.initialReorderModel
    }

subscriptions :: SyncDocumentRef -> [M.Sub Action]
subscriptions r = [subscribeDocument r UpdateDocument]

update :: SyncDocumentRef -> Action -> M.Effect p Model Action
update _ (EditField field value) = M.modify $ #editFields %~ Map.insert field value
update _ (UpdateDocument (DocumentChange newDocument _)) = do
  M.modify $ \s ->
    s
      & (#document .~ newDocument)
      & (#editFields %~ updateEditFields s.user newDocument)
  s <- M.get
  M.io_ $ M.consoleLog $ M.ms $ show s.document.lockedFields
update r (IssueCommand cmd) = M.io_ $ modifySyncDocument r cmd
update r (ReorderAction a) = do
  m <- M.get
  C.liftEffect #reorderFrom ReorderAction (C.updateReorderModel r (mkReorderCommand m) a)
  where
    mkReorderCommand :: Model -> CompetenceId -> Reorder Competence -> Maybe Command
    mkReorderCommand m id' to = do
      fromPosition <- orderPosition m.document.competences id'
      pure $ ReorderCompetence fromPosition to

updateEditFields
  :: User -> Document -> Map.Map ChangableField M.MisoString -> Map.Map ChangableField M.MisoString
updateEditFields u d m = Map.intersection m (mkEditFields u d)

mkEditFields :: User -> Document -> Map.Map ChangableField M.MisoString
mkEditFields u d =
  Map.toList d.lockedFields
    & filter (\(_, userId) -> userId == u.id)
    & map (\(field, _) -> (field, ""))
    & Map.fromList

view :: Model -> M.View m Action
view m =
  let title = editable V.title_ CompetenceGridTitle m
      description = editable V.text_ CompetenceGridDescription m
      competences =
        M.table_
          [T.tailwind [T.TableFixed, T.WFull]]
          [ M.colgroup_
              []
              [ M.col_ [M.colspan_ "1", T.tailwind [T.W24]]
              , M.col_ [M.colspan_ "1"]
              , M.col_ [M.colspan_ "1"]
              , M.col_ [M.colspan_ "1"]
              , M.col_ [M.colspan_ "1"]
              , M.col_ [M.colspan_ "1", T.tailwind [T.W8]]
              ]
          , M.thead_
              []
              [ M.tr_
                  []
                  [ M.th_ [T.tailwind [T.TableCell]] []
                  , M.th_ [T.tailwind [T.TableCell]] [M.text $ C.translate' C.LblCompetenceDescription]
                  , M.th_ [T.tailwind [T.TableCell]] [M.text $ C.translate' C.LblCompetenceBasicLevelDescription]
                  , M.th_ [T.tailwind [T.TableCell]] [M.text $ C.translate' C.LblCompetenceIntermediateLevelDescription]
                  , M.th_ [T.tailwind [T.TableCell]] [M.text $ C.translate' C.LblCompetenceAdvancedLevelDescription]
                  , M.th_ [T.tailwind [T.TableCell]] []
                  ]
              ]
          , M.tbody_ [] $ map (viewCompetence m) (ordered m.document.competences)
          ]
   in V.vBox_
        V.NoExpand
        (V.Expand V.Start)
        V.SmallGap
        [ title
        , description
        , competences
        ]

viewCompetence :: Model -> Competence -> M.View m Action
viewCompetence m c =
  let description = editable fmtText (CompetenceDescription c.id) m
      basicLevel = editable fmtText (CompetenceLevelDescription (c.id, BasicLevel)) m
      intermediateLevel = editable fmtText (CompetenceLevelDescription (c.id, IntermediateLevel)) m
      advancedLevel = editable fmtText (CompetenceLevelDescription (c.id, AdvancedLevel)) m
      reorderItemView = ReorderAction <$> C.viewReorderItem m.reorderFrom c
      fmtText t = M.span_ [T.tailwind [T.AlignMiddle]] [V.text_ t]
   in M.tr_
        []
        [ M.td_ [T.tailwind [T.TableCell, T.TextCenter]] [reorderItemView]
        , M.td_ [T.tailwind [T.TableCell]] [description]
        , M.td_ [T.tailwind [T.TableCell]] [basicLevel]
        , M.td_ [T.tailwind [T.TableCell]] [intermediateLevel]
        , M.td_ [T.tailwind [T.TableCell]] [advancedLevel]
        , M.td_ [T.tailwind [T.TableCell, T.TextCenter]] [V.deleteButton [M.onClick $ IssueCommand (RemoveCompetence c.id)]]
        ]

editable :: (M.MisoString -> M.View m Action) -> ChangableField -> Model -> M.View m Action
editable fmtText f m =
  let render :: M.View m Action -> [M.View m Action] -> M.View m Action
      render content buttons =
        V.hBox_ (V.Expand V.Start) V.NoExpand V.SmallGap $
          [V.growing_ [content]] <> buttons
      fieldText = fromMaybe "" $ m.document ^? fieldATraversal f
      inputField t =
        V.textarea_
          [M.id_ (M.ms $ show f), M.value_ (M.ms t), M.onInput (EditField f), T.tailwind [T.WFull]]
   in case editability m f of
        Editable ->
          render
            (fmtText $ M.ms fieldText)
            [V.editButton [M.onClick $ IssueCommand (LockField f m.user.id fieldText)]]
        LockedByUs t ->
          render
            (inputField t)
            [ V.applyButton [M.onClick $ IssueCommand (ReleaseField f (Just $ M.fromMisoString t))]
            , V.cancelButton [M.onClick $ IssueCommand (ReleaseField f Nothing)]
            ]
        LockedBy _u -> render (M.text $ M.ms fieldText) [] -- [lockedBy u]

data Editability
  = Editable
  | LockedBy !UserId
  | LockedByUs !M.MisoString

editability :: Model -> ChangableField -> Editability
editability m f =
  case Map.lookup f m.document.lockedFields of
    Nothing -> Editable
    Just lockedUid ->
      if lockedUid == m.user.id
        then LockedByUs $ fromMaybe "" $ Map.lookup f m.editFields
        else LockedBy lockedUid
