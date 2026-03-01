module Competences.Frontend.Component.Selector.LessonNotesSelector
  ( lessonNotesSelectorComponent
  )
where

import Competences.Command (EntityCommand (..), LessonNotesCommand (..))
import Competences.Command qualified as Cmd
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), LessonNotes (..), LessonNotesIxs)
import Competences.Document.LessonNotes (mkLessonNotes)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext
  , isInitialUpdate
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.SelectorList qualified as SL
import Competences.Frontend.View.Tailwind (class_)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (getCurrentTime, utctDay)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (ms)
import Optics.Core (Lens', toLensVL, (&), (.~), (?~))

data Model = Model
  { allNotes :: !(Ix.IxSet LessonNotesIxs LessonNotes)
  , selectedItem :: !(Maybe LessonNotes)
  , newItem :: !(Maybe LessonNotes)
  , searchQuery :: !Text
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectItem !LessonNotes
  | CreateLessonNotes
  | SetSearchQuery !Text
  | UpdateDocument !DocumentChange
  deriving (Eq, Show)

lessonNotesSelectorComponent
  :: SyncContext
  -> Bool
  -> Maybe (Ix.IxSet LessonNotesIxs LessonNotes -> Maybe LessonNotes)
  -> Lens' p (Maybe LessonNotes)
  -> M.Component p Model Action
lessonNotesSelectorComponent r canCreate initialSelection parentLens =
  (M.component model update view')
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedItem]
    , M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = Model Ix.empty Nothing Nothing ""

    update (SelectItem item) = M.modify $ \m ->
      case Ix.getOne (m.allNotes Ix.@= item.id) of
        Just ln -> m & (#selectedItem ?~ ln) & (#newItem .~ Nothing)
        Nothing -> m & (#newItem ?~ item)

    update CreateLessonNotes = M.withSink $ \s -> do
      lnId <- nextId r
      today <- utctDay <$> getCurrentTime
      let newLn = mkLessonNotes lnId today
      modifySyncDocument r $ Cmd.LessonNotes (OnLessonNotes (CreateAndLock newLn))
      s (SelectItem newLn)

    update (SetSearchQuery q) = M.modify $ \m ->
      m & #searchQuery .~ q

    update (UpdateDocument dc) = M.modify $ \m ->
      let allNotes' = dc.document.lessonNotes
          validatedSelected = case m.selectedItem of
            Just ln -> Ix.getOne (allNotes' Ix.@= ln.id)
            Nothing -> Nothing
          validatedNew = case m.newItem of
            Just ln ->
              case Ix.getOne (allNotes' Ix.@= ln.id) of
                Just ln' -> Just ln'
                Nothing -> m.newItem
            Nothing -> Nothing
          m' = m
            { allNotes = allNotes'
            , selectedItem = validatedSelected
            , newItem = validatedNew
            }
       in case (isInitialUpdate dc.change, m'.selectedItem, initialSelection) of
            (True, Nothing, Just f) ->
              m' {selectedItem = f allNotes'}
            _ -> m'

    view' m =
      M.div_
        [class_ "h-full"]
        [ Layout.vFlow
            (Layout.gapS <> Layout.hFull)
            [ SL.selectorHeader
                (C.translate' C.LblLessonNotesEntries)
                (if canCreate then Just CreateLessonNotes else Nothing)
            , SL.selectorSearchField (ms m.searchQuery) (C.translate' C.LblFilterLessonNotes) (SetSearchQuery . M.fromMisoString)
            , viewItems m
            ]
        ]

    viewItems m =
      let allItems = sortBy (comparing (.date) <> comparing (.title))
                       $ Ix.toList m.allNotes
          query = T.toLower m.searchQuery
          filteredItems =
            if T.null query
              then allItems
              else filter (\ln -> query `T.isInfixOf` T.toLower ln.title) allItems
       in SL.selectorList (map (viewItem m) filteredItems)

    viewItem m ln =
      let isSelected = m.selectedItem == Just ln || m.newItem == Just ln
          label = if T.null ln.title then "(Ohne Titel)" else ln.title
          dateTxt = C.formatDay ln.date
       in SL.selectorItemMultiLine
            isSelected
            [ M.span_ [class_ "text-sm font-medium truncate"] [M.text (ms label)]
            , M.span_ [class_ "text-xs text-muted-foreground"] [M.text dateTxt]
            ]
            (SelectItem ln)
