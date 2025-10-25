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
import Data.Time (Day, parseTimeM)
import Data.Time.Format (defaultTimeLocale)
import Data.Tuple (Solo (..))
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Html.Property qualified as M
import Optics.Core (Iso', Lens', (%), (%~), (&), (.~), (^.))
import Optics.Core qualified as O

data Model a f = Model
  { entries :: !(Maybe (f (a, Maybe UserId)))
  , patches :: !(Map.Map a a)
  , reorderFrom :: !(Maybe a)
  }
  deriving (Generic)

instance (Eq a, Functor f, Foldable f) => Eq (Model a f) where
  a == b =
    fmap toList a.entries == fmap toList b.entries &&
    a.patches == b.patches &&
    a.reorderFrom == b.reorderFrom

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

data EditorViewData a f n = EditorViewData
  { fields :: ![n]
  , items :: !(f (EditorViewItem a f n))
  }
  deriving (Generic)

data EditorViewItem a f n = EditorViewItem
  { item :: !a
  , editing :: !Bool
  , fieldData :: ![(n, M.View (Model a f) (Action a))]
  , itemActions :: ![M.View (Model a f) (Action a)]
  }
  deriving (Generic)

type EditorView a f n = EditorViewData a f n -> M.View (Model a f) (Action a)

withDeleteAction :: forall a f n. Functor f => (a -> Command) -> EditorView a f n -> EditorView a f n
withDeleteAction delete v = v . (#items %~ fmap addDeleteAction)
  where
    addDeleteAction :: EditorViewItem a f n -> EditorViewItem a f n
    addDeleteAction e = e & #itemActions %~ (<> [V.viewButton (V.deleteButton (IssueCommand $ delete e.item))])

data Editor a f n = Editor
  { editable :: !(Editable f a)
  , fields :: ![(n, EditorField a f)]
  , view :: !(EditorView a f n)
  }
  deriving (Generic)

data EditorField a f = EditorField
  { viewer :: !(a -> M.View (Model a f) (Action a))
  , editor :: !(a -> a -> M.View (Model a f) (Action a))
  }
  deriving (Generic)

editor :: EditorView a f n -> Editable f a -> Editor a f n
editor v e = Editor {editable = e, view = v, fields = []}

flowEditor :: Editable Solo a -> Editor a Solo n
flowEditor = editor editorFlowView

editorFlowView :: EditorView a Solo n
editorFlowView viewData =
  let (MkSolo item) = viewData ^. #items
   in V.viewFlow
        (V.hFlow & #gap .~ V.SmallSpace)
        ( map snd item.fieldData <> (^. #itemActions) item
        )

data TableRowEditorColumn n
  = TableRowEditorNamedColumn n
  | TableRowEditorActionColumn

editorTableRowView :: Foldable f => (n -> V.TableColumnSpec) -> V.TableColumnSpec -> EditorView a f n
editorTableRowView specOf actionSpec viewData =
  V.viewTable $
    V.Table
      { columns = map TableRowEditorNamedColumn viewData.fields <> [TableRowEditorActionColumn]
      , rows = toList viewData.items
      , columnSpec = \case
          TableRowEditorNamedColumn n -> specOf n
          TableRowEditorActionColumn -> actionSpec
      , rowContents = \_ r ->
          -- We know that cols matches the fields.
          V.tableRow $ map snd r.fieldData <> [V.viewFlow V.hFlow r.itemActions]
      }

editorTableRowView' :: Foldable f => EditorView a f M.MisoString
editorTableRowView' =
  editorTableRowView (V.TableColumnSpec V.AutoSizedColumn) (V.TableColumnSpec V.TripleActionColumn "")

editorFormView :: EditorView a Solo M.MisoString
editorFormView viewData = undefined

textEditorField :: Lens' a M.MisoString -> EditorField a f
textEditorField l =
  EditorField
    { viewer = textViewer (O.castOptic l)
    , editor = textEditor l
    }

dayEditorField :: Lens' a Day -> EditorField a f
dayEditorField l =
  let parseTime :: M.MisoString -> Maybe Day
      parseTime dateStr = parseTimeM False defaultTimeLocale "%Y-%m-%d" (M.fromMisoString dateStr)

      showTime :: Day -> M.MisoString
      showTime day = M.toMisoString $ show day
   in EditorField
        { viewer = \a -> M.input_ [M.type_ "date", M.value_ (showTime $ a ^. l), M.disabled_]
        , editor = \original patched ->
            M.input_
              [ M.type_ "date"
              , M.value_ (showTime $ patched ^. l)
              , M.onChange
                  ( \v -> case parseTime v of
                      Just v' -> UpdatePatch original (patched & (l .~ v'))
                      Nothing -> UpdatePatch original patched
                  )
              ]
        }

textViewer :: Lens' a M.MisoString -> a -> M.View (Model a f) (Action a)
textViewer l a =
  let s = a ^. l
   in V.text_ s

textEditor :: Lens' a M.MisoString -> a -> a -> M.View (Model a f) (Action a)
textEditor l original patched =
  M.input_ [M.onChange (\v -> UpdatePatch original (patched & (l .~ v))), M.value_ (O.view l patched)]

addField :: Editor a f () -> EditorField a f -> Editor a f ()
addField e f = e & #fields %~ (<> [((), f)])

addNamedField :: Editor a f n -> (n, EditorField a f) -> Editor a f n
addNamedField e f = e & #fields %~ (<> [f])

msIso :: O.Iso' Text M.MisoString
msIso = O.iso M.ms M.fromMisoString

editorComponent
  :: forall a f n p
   . (Functor f, Foldable f, Ord a) => Editor a f n -> SyncDocumentRef -> M.Component p (Model a f) (Action a)
editorComponent e r =
  (M.component model update view)
    { M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = Model Nothing Map.empty Nothing

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

    updateModel :: Document -> Model a f -> Model a f
    updateModel d (Model _ patches reorderFrom) =
      let es = e.editable.get d
          myEdits = filter (\(_, u) -> u == Just (syncDocumentEnv r ^. #connectedUser % #id)) (toList es)
          patches' = Map.fromList $ map (\(e', _) -> (e', fromMaybe e' (Map.lookup e' patches))) myEdits
       in Model (Just es) patches' reorderFrom

    view :: Model a f -> M.View (Model a f) (Action a)
    view (Model (Just entries) patches _) =
      e.view $
        EditorViewData
          { fields = map fst e.fields
          , items = fmap (viewItem patches) entries
          }
    view _ = M.div_ [] []

    viewItem :: Map.Map a a -> (a, Maybe UserId) -> EditorViewItem a f n
    viewItem patches (item, user)
      | user == Just (syncDocumentEnv r ^. #connectedUser % #id) =
          let item' = fromMaybe item (Map.lookup item patches)
           in EditorViewItem
                { item = item'
                , editing = True
                , fieldData = fmap (\(n, f) -> (n, f.editor item item')) e.fields
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
