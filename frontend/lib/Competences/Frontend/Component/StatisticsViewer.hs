module Competences.Frontend.Component.StatisticsViewer
  ( statisticsViewerComponent
  , Model (..)
  , Action (..)
  , emptyModel
  )
where

import Competences.Analysis.Statistics (userEvidenceByActivity, ActivityStats (..))
import Competences.Document (Document (..), User (..), emptyDocument)
import Competences.Document.Evidence (ActivityType (..))
import Competences.Document.User (UserId, isStudent)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.SyncDocument (SyncDocumentRef, subscribeDocument, DocumentChange (..))
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Table qualified as T
import Data.Foldable (toList)
import Data.List (find)
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.String (ms)
import Optics.Core ((&), (.~))

-- | Statistics Viewer Component Model
data Model = Model
  { document :: !Document
  , currentUserId :: !(Maybe UserId)
  }
  deriving (Eq, Generic, Show)

-- | Statistics Viewer Component Actions
data Action
  = UpdateDocument !DocumentChange
  | SetCurrentUser !(Maybe UserId)
  deriving (Eq, Show)

-- | Empty model for statistics viewer
emptyModel :: Model
emptyModel = Model
  { document = emptyDocument
  , currentUserId = Nothing
  }

-- | Statistics Viewer Component
statisticsViewerComponent :: SyncDocumentRef -> M.Component p Model Action
statisticsViewerComponent docRef =
  (M.component model update view)
    { M.subs = [subscribeDocument docRef UpdateDocument]
    , M.events = mempty
    , M.initialAction = Nothing
    }
  where
    model = emptyModel

    update :: Action -> M.Effect p Model Action
    update (UpdateDocument (DocumentChange doc _)) = do
      let -- Try to find the first student user as default
          firstStudent = case toList doc.users of
            (user:_) | isStudent user -> Just user.id
            _ -> Nothing
      M.modify $ \m -> m
        { document = doc
        , currentUserId = firstStudent
        }
    update (SetCurrentUser userId) = M.modify $ \m -> m { currentUserId = userId }

    view :: Model -> M.View Model Action
    view m =
      case m.currentUserId of
        Nothing -> noUserSelectedView
        Just userId ->
          case findUser userId m.document of
            Nothing -> userNotFoundView
            Just user -> statisticsView m.document user

    -- Helper to find a user by ID
    findUser userId doc =
      case toList (doc.users) of
        [] -> Nothing
        users -> find ((== userId) . (.id)) users

-- | View when no user is selected
noUserSelectedView :: M.View Model Action
noUserSelectedView =
  V.viewFlow V.vFlow
    [ V.text_ (C.translate' C.LblNoUser)
    , V.text_ "Please select a user from the user management screen."
    ]

-- | View when user is not found
userNotFoundView :: M.View Model Action
userNotFoundView =
  V.text_ (C.translate' C.LblNoUser)

-- | Main statistics view showing user evidence by activity
statisticsView :: Document -> User -> M.View Model Action
statisticsView doc user =
  let
      stats = userEvidenceByActivity doc user.id
      totalEvidence = sum (map (.count) stats)
      
      -- Create a row for each activity type
      activityRows = map activityStatRow stats
      
      -- Add a total row
      totalRow =
        T.tableRow
          [ V.text_ (C.translate' C.LblActivityType)
          , V.text_ (ms $ show totalEvidence)
          , V.text_ "-"  -- No weight for total
          ]
      
  in V.viewFlow V.vFlow
       [ -- Header with user info
         V.viewFlow (V.hFlow & (#gap .~ V.SmallSpace))
           [ V.text_ "Statistics for:"
           , V.text_ (ms user.name)
           ]
       , V.text_ " "  -- Simple spacing
       , -- Statistics table
         V.viewFlow V.vFlow activityRows
       , V.text_ " "
       , V.viewFlow V.vFlow [totalRow]
       ]

-- | Create a table row for a single activity statistic
activityStatRow :: ActivityStats -> M.View Model Action
activityStatRow stat =
  T.tableRow
    [ V.text_ (activityTypeText stat.activityType)
    , V.text_ (ms $ show stat.count)
    , V.text_ (ms $ show stat.weight)
    ]

-- | Convert activity type to display text
activityTypeText :: ActivityType -> M.MisoString
activityTypeText activityType = C.translate' (C.LblActivityTypeDescription activityType)
