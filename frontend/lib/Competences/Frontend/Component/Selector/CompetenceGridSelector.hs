module Competences.Frontend.Component.Selector.CompetenceGridSelector
  ( competenceGridSelectorComponent
  , CompetenceGridSelectorStyle (..)
  )
where

import Competences.Command qualified as Cmd
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( CompetenceGrid (..)
  , CompetenceGridId
  , CompetenceGridIxs
  , Document (..)
  , orderMax
  )
import Competences.Document.CompetenceGridGrade (CompetenceGridGrade (..), CompetenceGridGradeIxs)
import Competences.Document.Grade (Grade (..))
import Competences.Document.User (User (..), UserId)
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , FocusedUserChange (..)
  , SyncContext
  , getFocusedUserRef
  , modifySyncDocument
  , nextId
  , subscribeDocument
  , subscribeFocusedUser
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.SelectorList qualified as SL
import Competences.Frontend.View.Tailwind (class_)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core (Lens', toLensVL, (&), (.~), (?~))

data Model = Model
  { allCompetenceGrids :: !(Ix.IxSet CompetenceGridIxs CompetenceGrid)
  , competenceGridGrades :: !(Ix.IxSet CompetenceGridGradeIxs CompetenceGridGrade)
  , selectedCompetenceGrid :: !(Maybe CompetenceGrid)
  , newCompetenceGrid :: !(Maybe CompetenceGrid)
  , focusedUser :: !(Maybe User)
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectCompetenceGrid !CompetenceGrid
  | CreateNewCompetenceGrid
  | UpdateDocument !DocumentChange
  | FocusedUserChanged !FocusedUserChange
  deriving (Eq, Show)

data CompetenceGridSelectorStyle
  = CompetenceGridSelectorViewOnlyStyle
  | CompetenceGridSelectorViewAndCreateStyle
  deriving (Eq, Show)

competenceGridSelectorComponent
  :: SyncContext
  -> CompetenceGridSelectorStyle
  -> Lens' p (Maybe CompetenceGrid)
  -> M.Component p Model Action
competenceGridSelectorComponent r style parentLens =
  (M.component model update view)
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedCompetenceGrid]
    , M.subs =
        [ subscribeDocument r UpdateDocument
        , subscribeFocusedUser (getFocusedUserRef r) FocusedUserChanged
        ]
    }
  where
    model = Model Ix.empty Ix.empty Nothing Nothing Nothing
    update (SelectCompetenceGrid c) =
      M.modify $ \m -> case Ix.getOne (m.allCompetenceGrids Ix.@= c.id) of
        Just c' -> m & (#selectedCompetenceGrid ?~ c') & (#newCompetenceGrid .~ Nothing)
        Nothing -> m & (#newCompetenceGrid ?~ c)
    update CreateNewCompetenceGrid = M.withSink $ \s -> do
      competenceGridId <- nextId r
      let competenceGrid = CompetenceGrid competenceGridId orderMax "" ""
      modifySyncDocument r (Cmd.Competences $ Cmd.OnCompetenceGrids $ Cmd.Create competenceGrid)
      s (SelectCompetenceGrid competenceGrid)
    update (UpdateDocument (DocumentChange d _)) = M.modify $ updateModel d
    update (FocusedUserChanged change) = M.modify $ #focusedUser .~ change.user

    updateModel :: Document -> Model -> Model
    updateModel d m =
      let grids = d.competenceGrids
          grades = d.competenceGridGrades
          validateCompetenceGrid c = do
            c' <- c
            Ix.getOne $ grids Ix.@= c'.id
          (selected', new') = case (validateCompetenceGrid m.selectedCompetenceGrid, validateCompetenceGrid m.newCompetenceGrid) of
            (_, Just e) -> (Just e, Nothing)
            (s, n) -> (s, n)
       in m
            { allCompetenceGrids = grids
            , competenceGridGrades = grades
            , selectedCompetenceGrid = selected'
            , newCompetenceGrid = new'
            }

    view (m :: Model) =
      V.viewFlow
        ( V.vFlow
            & (#gap .~ V.SmallSpace)
            & (#expandDirection .~ V.Expand V.Start)
            & (#extraAttrs .~ [V.fullHeight])
        )
        [ SL.selectorHeader
            (C.translate' C.LblSelectCompetenceGrids)
            ( if style == CompetenceGridSelectorViewAndCreateStyle
                then Just CreateNewCompetenceGrid
                else Nothing
            )
        , SL.selectorList (map (viewCompetenceGrid m) (Ix.toList m.allCompetenceGrids))
        ]

    viewCompetenceGrid m c =
      let isSelected = m.selectedCompetenceGrid == Just c || m.newCompetenceGrid == Just c
          label = M.ms $ if c.title == "" then "Ohne Titel" else c.title
          -- Get active grade for this grid and focused user
          mGrade = do
            user <- m.focusedUser
            gridGrade <- getActiveGridGrade' m.competenceGridGrades user.id c.id
            pure gridGrade.grade
          gradeBadge = gradeBadgeView <$> mGrade
       in SL.selectorItemWithBadge isSelected IcnCompetenceGrid label gradeBadge (SelectCompetenceGrid c)

-- | Get the most recent (active) grid grade for a user and competence grid
getActiveGridGrade'
  :: Ix.IxSet CompetenceGridGradeIxs CompetenceGridGrade
  -> UserId
  -> CompetenceGridId
  -> Maybe CompetenceGridGrade
getActiveGridGrade' grades userId gridId =
  listToMaybe $ sortOn (Down . (.date)) $
    filter (\g -> g.competenceGridId == gridId) $
      Ix.toList (grades Ix.@= userId)

-- | Create a colored badge for a grade
-- Color coding: 1-3 green, 3-4/4/4-5 yellow, 5 red
gradeBadgeView :: Grade -> M.View m action
gradeBadgeView g =
  let (bgClass, textClass) = gradeColorClasses g
      shortLabel = gradeShortLabel g
   in MH.span_
        [ class_ $ "inline-flex items-center justify-center rounded-full px-2 py-0.5 text-xs font-medium " <> bgClass <> " " <> textClass
        ]
        [M.text (M.ms shortLabel)]

-- | Get background and text color classes for a grade
gradeColorClasses :: Grade -> (T.Text, T.Text)
gradeColorClasses g = case g of
  Grade1 -> ("bg-green-100", "text-green-700")
  Grade1_2 -> ("bg-green-100", "text-green-700")
  Grade2 -> ("bg-green-100", "text-green-700")
  Grade2_3 -> ("bg-green-100", "text-green-700")
  Grade3 -> ("bg-green-100", "text-green-700")
  Grade3_4 -> ("bg-yellow-100", "text-yellow-700")
  Grade4 -> ("bg-yellow-100", "text-yellow-700")
  Grade4_5 -> ("bg-yellow-100", "text-yellow-700")
  Grade5 -> ("bg-red-100", "text-red-700")

-- | Short label for grade (just the number part)
gradeShortLabel :: Grade -> T.Text
gradeShortLabel g = case g of
  Grade1 -> "1"
  Grade1_2 -> "1-2"
  Grade2 -> "2"
  Grade2_3 -> "2-3"
  Grade3 -> "3"
  Grade3_4 -> "3-4"
  Grade4 -> "4"
  Grade4_5 -> "4-5"
  Grade5 -> "5"
