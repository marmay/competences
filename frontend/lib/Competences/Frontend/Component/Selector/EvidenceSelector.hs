module Competences.Frontend.Component.Selector.EvidenceSelector
  ( evidenceSelectorComponent
  , EvidenceSelectorStyle (..)
  )
where

import Competences.Command qualified as Cmd
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Evidence, EvidenceIxs, User (..))
import Competences.Document.Evidence
  ( Evidence (..)
  , mkEvidence
  )
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Selector.EnumSelector qualified as ES
-- Note: User selector removed - now uses global focused user from nav bar
import Competences.Frontend.SyncDocument
  ( DocumentChange (..)
  , FocusedUserChange (..)
  , SyncDocumentEnv (..)
  , SyncDocumentRef
  , modifySyncDocument
  , nextId
  , subscribeDocument
  , subscribeFocusedUser
  , syncDocumentEnv
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.SelectorList qualified as SL
import Data.Proxy (Proxy (..))
import Data.Time (Day, addDays)
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core (Lens', toLensVL, (&), (.~), (?~), (^.))

data EvidenceSelectorStyle
  = EvidenceSelectorViewOnly
  | EvidenceSelectorViewAndCreate
  deriving (Eq, Show)

data DateRange
  = Today
  | ThisWeek
  | AllTime
  deriving (Eq, Show)

data Model = Model
  { focusedUser :: !(Maybe User)  -- From global focused user subscription
  , filteredDateRange :: !DateRange
  , allEvidences :: Ix.IxSet EvidenceIxs Evidence
  , selectedEvidence :: !(Maybe Evidence)
  , newEvidence :: !(Maybe Evidence)
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectEvidence !Evidence
  | CreateNewEvidence
  | UpdateDocument !DocumentChange
  | FocusedUserChanged !FocusedUserChange
  deriving (Eq, Show)

evidenceSelectorComponent
  :: SyncDocumentRef -> EvidenceSelectorStyle -> Lens' p (Maybe Evidence) -> M.Component p Model Action
evidenceSelectorComponent r style parentLens =
  (M.component model update view)
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedEvidence]
    , M.subs =
        [ subscribeDocument r UpdateDocument
        , subscribeFocusedUser r FocusedUserChanged
        ]
    }
  where
    model = Model Nothing AllTime Ix.empty Nothing Nothing
    update (SelectEvidence e) =
      M.modify $ \m -> case Ix.getOne (m.allEvidences Ix.@= e.id) of
        Just e' -> m & (#selectedEvidence ?~ e') & (#newEvidence .~ Nothing)
        Nothing -> m & (#newEvidence ?~ e)
    update CreateNewEvidence = M.withSink $ \s -> do
      evidenceId <- nextId r
      let today = syncDocumentEnv r ^. #currentDay
      let evidence = mkEvidence evidenceId today
      modifySyncDocument r (Cmd.Evidences $ Cmd.OnEvidences $ Cmd.CreateAndLock evidence)
      s (SelectEvidence evidence)
    update (UpdateDocument (DocumentChange d _)) = M.modify $ updateModel d
    update (FocusedUserChanged change) =
      M.modify $ \m -> m & #focusedUser .~ change.user

    updateModel :: Document -> Model -> Model
    updateModel d m =
      let allEvidences' = d.evidences
          validateEvidence e = do
            e' <- e
            Ix.getOne $ allEvidences' Ix.@= e'.id
          (selectedEvidence', newEvidence') = case (validateEvidence m.selectedEvidence, validateEvidence m.newEvidence) of
            (_, Just e) -> (Just e, Nothing)
            (s, n) -> (s, n)
       in m
            { allEvidences = allEvidences'
            , selectedEvidence = selectedEvidence'
            , newEvidence = newEvidence'
            }

    view m =
      V.viewFlow
        ( V.vFlow
            & (#gap .~ V.SmallSpace)
            & (#expandDirection .~ V.Expand V.Start)
            & (#extraAttrs .~ [V.fullHeight])
        )
        [ SL.selectorHeader
            (C.translate' C.LblSelectEvidences)
            (if style == EvidenceSelectorViewAndCreate then Just CreateNewEvidence else Nothing)
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
      let dateRangeFilter :: Ix.IxSet EvidenceIxs Evidence -> Ix.IxSet EvidenceIxs Evidence
          dateRangeFilter = case m.filteredDateRange of
            AllTime -> id
            ThisWeek -> (Ix.@>= (addDays (-7) $ syncDocumentEnv r ^. #currentDay))
            Today -> (Ix.@= (syncDocumentEnv r ^. #currentDay))
          -- Use global focused user for filtering (from nav bar)
          filteredEvidences =
            Ix.toDescList (Proxy @Day) (dateRangeFilter $ m.allEvidences Ix.@=! fmap (.id) m.focusedUser)
       in SL.selectorList (map (viewEvidence m) filteredEvidences)

    viewEvidence m e =
      let isSelected = m.selectedEvidence == Just e || m.newEvidence == Just e
          label = C.formatDay e.date <> " — " <> C.translate' (C.LblActivityTypeDescription e.activityType)
       in SL.selectorItem isSelected IcnEvidence label (SelectEvidence e)

    translateDateRange Today = C.translate' C.LblToday
    translateDateRange ThisWeek = C.translate' C.LblThisWeek
    translateDateRange AllTime = C.translate' C.LblAllTime
