-- | Evidence list selector — config wrapper around
-- 'listSelectorComponent'.
--
-- Evidences are pre-filtered to the focused user in the projection.
-- Sorted by date (most recent first); no filter UI and no create
-- entries — manual evidence creation is no longer a flow (evidences
-- are produced via assignments). View-only.
module Competences.Frontend.Component.Evidence.ListSelector
  ( evidenceListSelectorComponent
  )
where

import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), EvidenceIxs, User (..))
import Competences.Document.Evidence (Evidence (..), EvidenceId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Selector.List
  ( Action (..)
  , ItemRenderer (..)
  , ListSelectorConfig (..)
  , Model
  , listSelectorComponent
  )
import Competences.Frontend.Component.Selector.UriBinding (pageBinding)
import Competences.Frontend.Fragment.SelectorFilter (noopFilter)
import Competences.Frontend.Page (Page (..))
import Competences.Frontend.SyncContext (SyncContext)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.SelectorList qualified as SL
import Competences.Query.Evidence qualified as QEvidence
import Data.Proxy (Proxy (..))
import Data.Time (Day)
import GHC.Generics (Generic)
import Miso qualified as M
import Optics.Core (Lens')

type Selected = Evidence

-- | Projection: evidences pre-filtered to the focused user.
data Projection = Projection
  { userEvidences :: !(Ix.IxSet EvidenceIxs Evidence)
  , focusedUser :: !(Maybe User)
  }
  deriving (Eq, Generic, Show)

emptyProjection :: Projection
emptyProjection = Projection Ix.empty Nothing

projectEvidences :: Document -> Maybe User -> Projection
projectEvidences doc mUser =
  Projection
    { userEvidences = case mUser of
        Nothing -> Ix.empty
        Just u -> QEvidence.userEvidences doc u.id
    , focusedUser = mUser
    }

evidenceListSelectorComponent
  :: SyncContext
  -> Maybe EvidenceId
  -> Lens' p (Maybe Selected)
  -> M.Component p (Model Selected Projection ()) (Action Selected Projection ())
evidenceListSelectorComponent r mDeepLink parentLens =
  listSelectorComponent r (config mDeepLink parentLens)

config
  :: Maybe EvidenceId
  -> Lens' p (Maybe Selected)
  -> ListSelectorConfig p Selected Projection EvidenceIxs EvidenceId () ()
config mDeepLink parentLens =
  ListSelectorConfig
    { title = C.translate' C.LblSelectEvidences
    , project = projectEvidences
    , emptyProjection = emptyProjection
    , entitiesOf = (.userEvidences)
    , itemsInOrder = Ix.toDescList (Proxy @Day)
    , idOf = (.id)
    , itemView = ItemRenderer renderItem
    , createActions = []
    , uriBinding =
        Just $ pageBinding Evidences $ \case
          Evidences mEid -> Just mEid
          _ -> Nothing
    , initialPick = case mDeepLink of
        Just eid -> Just (\xs -> Ix.getOne (xs Ix.@= eid))
        Nothing -> Nothing
    , filter = noopFilter
    , parentLens = parentLens
    }

renderItem
  :: Selected
  -> Projection
  -> Bool
  -> M.View m (Action Selected Projection ())
renderItem e _proj isSel =
  let label = C.formatDay e.date <> " — " <> C.translate' (C.LblActivityTypeDescription e.activityType)
   in SL.selectorItem isSel Icon.IcnEvidence label (Pick e)
