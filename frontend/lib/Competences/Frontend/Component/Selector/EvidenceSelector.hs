module Competences.Frontend.Component.Selector.EvidenceSelector
  ( evidenceSelectorComponent
  , EvidenceSelectorStyle (..)
  , Model (..)
  , Action (..)
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
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncDocumentEnv (..)
  , SyncContext
  , modifySyncDocument
  , nextId
  , subscribeWithProjection
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

-- | Projection type: extracts only the data needed for this component.
-- Evidence is pre-filtered to the focused user.
data EvidenceSelectorProjection = EvidenceSelectorProjection
  { userEvidences :: !(Ix.IxSet EvidenceIxs Evidence)
  -- ^ Evidences filtered to focused user only
  , focusedUser :: !(Maybe User)
  }
  deriving (Eq, Generic, Show)

-- | Compute the projection from document and focused user.
-- Pre-filters evidences to the focused user for efficient view rendering.
evidenceSelectorProjection :: Document -> Maybe User -> EvidenceSelectorProjection
evidenceSelectorProjection doc mUser = EvidenceSelectorProjection
  { userEvidences = case mUser of
      Nothing -> Ix.empty
      Just u -> doc.evidences Ix.@= u.id
  , focusedUser = mUser
  }

data DateRange
  = Today
  | ThisWeek
  | AllTime
  deriving (Eq, Show)

data Model = Model
  { projection :: !EvidenceSelectorProjection
  , filteredDateRange :: !DateRange
  , selectedEvidence :: !(Maybe Evidence)
  , newEvidence :: !(Maybe Evidence)
  , dropdownOpen :: !Bool
  , bulkEditorActive :: !Bool
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectEvidence !Evidence
  | CreateNewEvidence
  | ProjectionChanged !(ProjectedChange EvidenceSelectorProjection)
  | ToggleDropdown
  | CloseDropdown
  | ActivateBulkEditor
  | DeactivateBulkEditor
  deriving (Eq, Show)

-- | Evidence selector component with dropdown menu for normal and bulk creation
evidenceSelectorComponent
  :: SyncContext
  -> EvidenceSelectorStyle
  -> Lens' p (Maybe Evidence)
  -> Lens' p Bool  -- ^ Lens for bulkEditorActive state in parent
  -> M.Component p Model Action
evidenceSelectorComponent r style parentLens bulkEditorLens =
  (M.component model update view)
    { M.bindings =
        [ toLensVL parentLens M.<--- toLensVL #selectedEvidence
        , toLensVL bulkEditorLens M.<--- toLensVL #bulkEditorActive
        ]
    , M.subs = [subscribeWithProjection r evidenceSelectorProjection ProjectionChanged]
    }
  where
    model = Model (EvidenceSelectorProjection Ix.empty Nothing) AllTime Nothing Nothing False False
    update (SelectEvidence e) =
      M.modify $ \m -> case Ix.getOne (m.projection.userEvidences Ix.@= e.id) of
        Just e' -> m & (#selectedEvidence ?~ e') & (#newEvidence .~ Nothing) & (#bulkEditorActive .~ False)
        Nothing -> m & (#newEvidence ?~ e) & (#bulkEditorActive .~ False)
    update CreateNewEvidence = M.withSink $ \s -> do
      evidenceId <- nextId r
      let today = syncDocumentEnv r ^. #currentDay
      let evidence = mkEvidence evidenceId today
      modifySyncDocument r (Cmd.Evidences $ Cmd.OnEvidences $ Cmd.CreateAndLock evidence)
      s CloseDropdown
      s (SelectEvidence evidence)
    update (ProjectionChanged change) = M.modify $ updateFromProjection change.projection
    update ToggleDropdown =
      M.modify $ \m -> m & #dropdownOpen .~ not m.dropdownOpen
    update CloseDropdown =
      M.modify $ \m -> m & #dropdownOpen .~ False
    update ActivateBulkEditor =
      M.modify $ \m -> m & #bulkEditorActive .~ True & #selectedEvidence .~ Nothing & #newEvidence .~ Nothing & #dropdownOpen .~ False
    update DeactivateBulkEditor =
      M.modify $ \m -> m & #bulkEditorActive .~ False

    updateFromProjection :: EvidenceSelectorProjection -> Model -> Model
    updateFromProjection proj m =
      let evidences = proj.userEvidences
          validateEvidence e = do
            e' <- e
            Ix.getOne $ evidences Ix.@= e'.id
          (selectedEvidence', newEvidence') = case (validateEvidence m.selectedEvidence, validateEvidence m.newEvidence) of
            (_, Just e) -> (Just e, Nothing)
            (s, n) -> (s, n)
       in m
            { projection = proj
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
        [ if style == EvidenceSelectorViewAndCreate
            then
              SL.selectorHeaderWithDropdown
                (C.translate' C.LblSelectEvidences)
                m.dropdownOpen
                ToggleDropdown
                [ SL.dropdownItem IcnAdd (C.translate' C.LblNewEvidence) CreateNewEvidence
                , SL.dropdownItem IcnEvidence (C.translate' C.LblBulkEntry) ActivateBulkEditor
                ]
            else
              SL.selectorHeader (C.translate' C.LblSelectEvidences) Nothing
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
          -- userEvidences is already filtered to focused user in projection
          filteredEvidences =
            Ix.toDescList (Proxy @Day) (dateRangeFilter m.projection.userEvidences)
       in SL.selectorList (map (viewEvidence m) filteredEvidences)

    viewEvidence m e =
      let isSelected = m.selectedEvidence == Just e || m.newEvidence == Just e
          label = C.formatDay e.date <> " — " <> C.translate' (C.LblActivityTypeDescription e.activityType)
       in SL.selectorItem isSelected IcnEvidence label (SelectEvidence e)

    translateDateRange Today = C.translate' C.LblToday
    translateDateRange ThisWeek = C.translate' C.LblThisWeek
    translateDateRange AllTime = C.translate' C.LblAllTime
