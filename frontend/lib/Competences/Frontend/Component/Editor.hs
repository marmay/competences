module Competences.Frontend.Component.Editor
  ( editor
  , flowEditor
  , addField
  , addNamedField
  , Editor
  , Editable (..)
  , EditorField
  , editorTableRowView
  , editorTableRowView'
  , textEditorField
  , editorComponent
  , msIso
  , withDeleteAction
  )
where

import Competences.Command (Command (..), ModifyCommand (..))
import Competences.Document (Document (..), User (..), UserId)
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
import Data.Tuple (Solo)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as M
import Optics.Core (Iso', Lens', (%), (%~), (&), (.~), (^.))
import Optics.Core qualified as O

data Model a = Model
  { entries :: ![(a, Maybe UserId)]
  , patches :: !(Map.Map a a)
  , reorderFrom :: !(Maybe a)
  }
  deriving (Eq, Show, Generic)

data Action a
  = StartEditing !a
  | CancelEditing !a
  | FinishEditing !a
  | UpdatePatch !a !a
  | UpdateDocument !DocumentChange
  | IssueCommand !Command

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
    addDeleteAction e = e & #itemActions %~ (<> [V.viewButton (V.deleteButton (IssueCommand $ delete e.item))])

data Editor a f n = Editor
  { editable :: !(Editable f a)
  , fields :: ![(n, EditorField a)]
  , view :: !(EditorView a n)
  }
  deriving (Generic)

data EditorField a = EditorField
  { viewer :: !(a -> M.View (Model a) (Action a))
  , editor :: !(a -> a -> M.View (Model a) (Action a))
  }
  deriving (Generic)

editor :: EditorView a n -> Editable f a -> Editor a f n
editor v e = Editor {editable = e, view = v, fields = []}

flowEditor :: Editable Solo a -> Editor a Solo n
flowEditor = editor editorFlowView

editorFlowView :: EditorView a n
editorFlowView viewData =
  let items = (viewData ^. #items)
   in V.viewFlow
        (V.hFlow & #gap .~ V.SmallSpace)
        ( map snd (concatMap (^. #fieldData) items)
            <> concatMap (^. #itemActions) items
        )

data TableRowEditorColumn n
  = TableRowEditorNamedColumn n
  | TableRowEditorActionColumn

editorTableRowView :: (n -> V.TableColumnSpec) -> V.TableColumnSpec -> EditorView a n
editorTableRowView specOf actionSpec viewData =
  V.viewTable $
    V.Table
      { columns = map TableRowEditorNamedColumn viewData.fields <> [TableRowEditorActionColumn]
      , rows = viewData.items
      , columnSpec = \case
          TableRowEditorNamedColumn n -> specOf n
          TableRowEditorActionColumn -> actionSpec
      , rowContents = \_ r ->
          -- We know that cols matches the fields.
          V.tableRow $ map snd r.fieldData <> [V.viewFlow V.hFlow r.itemActions]
      }

editorTableRowView' :: EditorView a M.MisoString
editorTableRowView' =
  editorTableRowView (V.TableColumnSpec V.AutoSizedColumn) (V.TableColumnSpec V.TripleActionColumn "")

textEditorField :: Lens' a M.MisoString -> EditorField a
textEditorField l =
  EditorField
    { viewer = textViewer (O.castOptic l)
    , editor = textEditor l
    }

textViewer :: Lens' a M.MisoString -> a -> M.View (Model a) (Action a)
textViewer l a =
  let s = a ^. l
   in V.text_ s

textEditor :: Lens' a M.MisoString -> a -> a -> M.View (Model a) (Action a)
textEditor l original patched =
  M.input_ [M.onChange (\v -> UpdatePatch original (patched & (l .~ v))), M.value_ (O.view l patched)]

addField :: Editor a f () -> EditorField a -> Editor a f ()
addField e f = e & #fields %~ (<> [((), f)])

addNamedField :: Editor a f n -> (n, EditorField a) -> Editor a f n
addNamedField e f = e & #fields %~ (<> [f])

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
    model = Model [] Map.empty Nothing

    update (StartEditing a) =
      M.io_ (modifySyncDocument r (e.editable.modify a Lock))
    update (CancelEditing a) =
      M.io_ (modifySyncDocument r (e.editable.modify a $ Release Nothing))
    update (FinishEditing a) = do
      patches <- (^. #patches) <$> M.get
      case patches Map.!? a of
        Just t -> M.io_ (modifySyncDocument r (e.editable.modify a $ Release $ Just (a, t)))
        Nothing -> pure ()
    update (UpdatePatch original patched) =
      M.modify $ #patches %~ Map.insert original patched
    update (UpdateDocument (DocumentChange newDocument _)) =
      M.modify $ updateModel newDocument
    update (IssueCommand cmd) =
      M.io_ (modifySyncDocument r cmd)

    updateModel :: Document -> Model a -> Model a
    updateModel d (Model _ patches reorderFrom) =
      let (es :: [(a, Maybe UserId)]) = toList $ e.editable.get d
          myEdits = filter (\(_, u) -> u == Just (syncDocumentEnv r ^. #connectedUser % #id)) es
          patches' = Map.fromList $ map (\(e', _) -> (e', fromMaybe e' (Map.lookup e' patches))) myEdits
       in Model es patches' reorderFrom

    view (Model entries patches _) =
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
                , fieldData = map (\(n, f) -> (n, f.editor item item')) e.fields
                , itemActions =
                    [ V.viewButton (V.applyButton (FinishEditing item))
                    , V.viewButton (V.cancelButton (CancelEditing item))
                    ]
                }
      | otherwise =
          EditorViewItem
            { item = item
            , editing = False
            , fieldData = map (\(n, f) -> (n, f.viewer item)) e.fields
            , itemActions = [V.viewButton (V.editButton (StartEditing item))]
            }
