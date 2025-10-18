module Competences.Frontend.Component.Editor
  ( editor
  , flowEditor
  , addField
  , Editor
  , Editable(..)
  , EditorField
  , textEditorField
  , editorComponent
  , msIso
  )
where

import Competences.Command (Command (..), ModifyCommand (..))
import Competences.Document (CompetenceGrid (..), Document (..), Lock (..), User (..), UserId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , SyncDocumentEnv (..)
  , SyncDocumentRef
  , modifySyncDocument
  , subscribeDocument
  , syncDocumentEnv
  )
import Competences.Frontend.View qualified as V
import Data.Foldable (toList)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as M
import Optics.Core (Iso', Lens', (%), (%~), (&), (.~), (^.))
import Optics.Core qualified as O

data Model a = Model
  { entries :: ![(a, Maybe UserId)]
  , patches :: !(Map.Map a a)
  }
  deriving (Eq, Show, Generic)

data Action a
  = StartEditing !a
  | CancelEditing !a
  | FinishEditing !a
  | UpdatePatch !a !a
  | UpdateDocument !DocumentChange
  | IssueCommand !Command
  deriving (Eq, Show)

data Editable f a = Editable
  { get :: !(Document -> f (a, Maybe UserId))
  , modify :: !(a -> ModifyCommand a -> Command)
  }

data EditorViewData a n = EditorViewData
  { fields :: ![n]
  , items :: ![EditorViewItem a n]
  }
  deriving (Generic)

data EditorViewItem a n = EditorViewItem
  { item :: !a
  , editing :: !Bool
  , fieldData :: ![(n, M.View (Model a) (Action a))]
  , itemActions :: ![M.View (Model a) (Action a)]
  }
  deriving (Generic)

type EditorView a n = EditorViewData a n -> M.View (Model a) (Action a)

withDeleteAction :: forall a n. (a -> Command) -> EditorView a n -> EditorView a n
withDeleteAction delete v = v . (#items %~ fmap addDeleteAction)
  where
    addDeleteAction :: EditorViewItem a n -> EditorViewItem a n
    addDeleteAction e = e & (#itemActions %~ (<> [V.viewButton (V.deleteButton (IssueCommand $ delete e.item))]))

data Editor a f n = Editor
  { editable :: !(Editable f a)
  , fields :: ![(n, EditorField a)]
  , view :: !(EditorView a n)
  }
  deriving (Generic)

data EditorField a = EditorField
  { viewer :: !(a -> M.View (Model a) (Action a))
  , editor :: !(a -> M.View (Model a) (Action a))
  }
  deriving (Generic)

editor :: EditorView a n -> Editable f a -> Editor a f n
editor v e = Editor {editable = e, view = v, fields = []}

flowEditor :: Editable f a -> Editor a f n
flowEditor = editor flowView
  where
    flowView viewData =
      let items = (viewData ^. #items)
       in V.viewFlow
            (V.hFlow & (#gap .~ V.SmallSpace))
            ( map snd (concatMap (^. #fieldData) items)
                <> concatMap (^. #itemActions) items
            )

tableRowEditor :: Editable f a -> Editor a f n
tableRowEditor = editor tableRowView
  where
    tableRowView = undefined

textEditorField :: Iso' a M.MisoString -> EditorField a
textEditorField l =
  EditorField
    { viewer = textViewer (O.castOptic l)
    , editor = textEditor l
    }

textViewer :: Lens' a M.MisoString -> a -> M.View (Model a) (Action a)
textViewer l a =
  let s = a ^. l
   in V.text_ s

textEditor :: Iso' a M.MisoString -> a -> M.View (Model a) (Action a)
textEditor l a =
  M.input_ [M.onChange (UpdatePatch a . O.review l), M.value_ (O.view l a)]

addField :: Editor a f () -> EditorField a -> Editor a f ()
addField e f = e & (#fields %~ (<> [((), f)]))

addNamedField :: Editor a f n -> (n, EditorField a) -> Editor a f n
addNamedField e f = e & (#fields %~ (<> [f]))

msIso :: O.Iso' Text M.MisoString
msIso = O.iso M.ms M.fromMisoString

editorComponent
  :: forall a f n p
   . (Foldable f, Ord a) => Editor a f n -> SyncDocumentRef -> M.Component p (Model a) (Action a)
editorComponent e r =
  (M.component model update view)
    { M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = Model [] Map.empty

    update (StartEditing a) =
      M.io_ (modifySyncDocument r (e.editable.modify a Lock))
    update (CancelEditing a) =
      M.io_ (modifySyncDocument r (e.editable.modify a $ Release Nothing))
    update (FinishEditing a) = do
      patches <- (^. #patches) <$> M.get
      case patches Map.!? a of
        Just t -> M.io_ (modifySyncDocument r (e.editable.modify a $ Release $ Just (a, t)))
        Nothing -> pure ()
    update (UpdatePatch a a') = do
      M.modify $ #patches %~ Map.insert a a'
    update (UpdateDocument (DocumentChange newDocument _)) =
      M.modify $ updateModel newDocument
    update (IssueCommand cmd) =
      M.io_ (modifySyncDocument r cmd)

    updateModel :: Document -> Model a -> Model a
    updateModel d (Model _ patches) =
      let (es :: [(a, Maybe UserId)]) = toList $ e.editable.get d
          myEdits = filter (\(_, u) -> u == Just (syncDocumentEnv r ^. #connectedUser % #id)) es
          patches' = Map.fromList $ map (\(e', _) -> (e', fromMaybe e' (Map.lookup e' patches))) myEdits
       in Model es patches'

    view (Model entries patches) =
      e.view $
        EditorViewData
          { fields = map fst e.fields
          , items = map (viewItem patches) entries
          }

    viewItem :: Map.Map a a -> (a, Maybe UserId) -> EditorViewItem a n
    viewItem patches (item, user)
      | user == Just (syncDocumentEnv r ^. #connectedUser % #id) =
          let item' = fromMaybe item (Map.lookup item patches)
           in EditorViewItem
                { item = item'
                , editing = True
                , fieldData = mkFieldData (.editor) item'
                , itemActions =
                    [ V.viewButton (V.applyButton (FinishEditing item))
                    , V.viewButton (V.cancelButton (CancelEditing item))
                    ]
                }
      | otherwise =
          EditorViewItem
            { item = item
            , editing = False
            , fieldData = mkFieldData (.viewer) item
            , itemActions = [V.viewButton (V.editButton (StartEditing item))]
            }

    mkFieldData
      :: (EditorField a -> (a -> M.View (Model a) (Action a))) -> a -> [(n, M.View (Model a) (Action a))]
    mkFieldData f a = map (\(n, e') -> (n, f e' a)) e.fields
