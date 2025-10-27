module Competences.Frontend.Component.Selector.EvidenceSelector
  ( evidenceSelectorComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Evidence, EvidenceIxs, User (..), UserId)
import Competences.Document.Evidence (ActivityTasks (..), ActivityType (..), Evidence (..))
import Competences.Document.User (isStudent)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Selector.EnumSelector qualified as ES
import Competences.Frontend.Component.Selector.UserSelector (singleUserSelectorComponent)
import Competences.Frontend.SyncDocument (DocumentChange (..), SyncDocumentRef)
import Competences.Frontend.View qualified as V
import Data.Foldable (toList)
import Data.List (sort)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Optics.Core (Lens', toLensVL, (&), (.~), (?~))

data DateRange
  = Today
  | ThisWeek
  | AllTime
  deriving (Eq, Show)

data Model = Model
  { filteredUsers :: !(Maybe User)
  , filteredDateRange :: !DateRange
  , allEvidences :: Ix.IxSet EvidenceIxs Evidence
  , userNames :: Map.Map UserId M.MisoString
  , selectedEvidence :: !(Maybe Evidence)
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectEvidence !Evidence
  | UpdateDocument !DocumentChange
  deriving (Eq, Show)

evidenceSelectorComponent
  :: SyncDocumentRef -> Lens' p (Maybe Evidence) -> M.Component p Model Action
evidenceSelectorComponent r parentLens =
  (M.component model update view)
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedEvidence]
    }
  where
    model = Model Nothing ThisWeek Ix.empty Map.empty Nothing
    update (SelectEvidence e) = M.modify $ #selectedEvidence ?~ e
    update (UpdateDocument (DocumentChange d _)) = M.modify $ updateModel d

    updateModel :: Document -> Model -> Model
    updateModel d m =
      let allEvidences' = d.evidences
          userNames' = Map.fromList $ map (\u -> (u.id, M.ms u.name)) (Ix.toList d.users)
          selectedEvidence' = do
            s <- m.selectedEvidence
            Ix.getOne $ allEvidences' Ix.@= s.id
       in m {allEvidences = allEvidences', userNames = userNames', selectedEvidence = selectedEvidence'}

    view m =
      V.viewFlow
        (V.vFlow & (#gap .~ V.SmallSpace))
        [ V.title_ "Evidences"
        , V.component "evidence-selector-users" (singleUserSelectorComponent r isStudent #filteredUsers)
        , V.component
            "evidence-selector-date-range"
            ( ES.enumSelectorComponent'
                AllTime
                [Today, ThisWeek, AllTime]
                ES.ButtonsCompact
                translateDateRange
                #filteredDateRange
            )
        , viewEvidences m
        ]
    viewEvidences m =
      let filteredEvidences = Ix.toList m.allEvidences
       in V.viewFlow
            V.vFlow
            (map viewEvidence filteredEvidences)
      where
        viewEvidence e =
          M.a_
            [C.onClick' (SelectEvidence e)]
            [ V.viewFlow
                (V.vFlow & #expandOrthogonal .~ V.Expand V.Start)
                ( V.viewFlow
                    (V.hFlow & (#expandDirection .~ V.Expand V.Start))
                    [ viewDate e.date
                    , V.flowSpring
                    , viewActivityType e.activityType
                    ]
                    : [ viewContext [] (sort $ mapMaybe (`Map.lookup` m.userNames) (toList e.userIds))
                      , viewContext [] [viewActivityTasks e.activityTasks]
                      ]
                )
            ]
        viewDate d = M.text_ [C.formatDay d]
        viewActivityType Supervised = "Supervised"
        viewActivityType SemiSupervised = "Semi-Supervised"
        viewActivityType Unsupervised = "Unsupervised"
        viewActivityTasks (ActivityTasks t) = M.ms t
        viewContext extraAttrs ms = M.span_ ([] <> extraAttrs) [M.text_ ms]

    translateDateRange Today = C.translate' C.LblToday
    translateDateRange ThisWeek = C.translate' C.LblThisWeek
    translateDateRange AllTime = C.translate' C.LblAllTime
