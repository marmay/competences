module Competences.Frontend.App
  ( runApp
  , mkApp
  , withTailwindPlay
  )
where

import Competences.Document.User (User)
import Competences.Frontend.Page.EditCompetenceGridPage
  ( EditCompetenceGridView
  , editCompetenceGridPage
  )
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Competences.Frontend.View qualified as V
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Language.Javascript.JSaddle (JSM)
import Miso qualified as M
import Optics.Core ((.~), (^.))
import Servant.API ((:<|>) (..), (:>))
import Servant.Links (allLinks', linkURI)

type App = M.Component M.ROOT Model Action

newtype Model = Model
  { uri :: M.URI
  }
  deriving (Eq, Generic, Show)

data Action
  = PushURI M.URI
  | SetURI M.URI
  deriving (Eq, Show)

runApp :: App -> JSM ()
runApp = M.startComponent

mkApp :: SyncDocumentRef -> User -> App
mkApp r u =
  M.component model update view
  where
    model = Model editCompetenceGridUri

    update (SetURI uri) = M.modify $ #uri .~ uri
    update (PushURI uri) = M.io_ $ M.pushURI uri

    view m =
      M.div_
        []
        [ V.iconDefs
        , V.vBox_ (V.Expand V.Start) (V.Expand V.Center) V.SmallGap [topMenu, page (m ^. #uri), footer]
        ]

    topMenu = V.hBox_ (V.Expand V.Start) V.NoExpand V.SmallGap [V.text_ "Top Menu"]

    page uri =
      case M.route (Proxy @API) (viewCompetenceGrid :<|> editCompetenceGrid) id uri of
        Left _ -> V.text_ "404"
        Right v -> v

    viewCompetenceGrid _ = V.text_ "View Competence Grid"
    editCompetenceGrid _ = mounted $ editCompetenceGridPage r u

    mounted c = M.div_ [] M.+> c

    footer = V.text_ "Footer"

withTailwindPlay :: App -> App
withTailwindPlay app = app {M.scripts = M.Src "https://cdn.tailwindcss.com" : M.scripts app}

type ViewCompetenceGridView = EditCompetenceGridView

type API = ("view" :> ViewCompetenceGridView) :<|> EditCompetenceGridView

viewCompetenceGridUri, editCompetenceGridUri :: M.URI
viewCompetenceGridUri :<|> editCompetenceGridUri = allLinks' linkURI (Proxy @API)
