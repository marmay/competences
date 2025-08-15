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
import Competences.Frontend.Page.ViewCompetenceGridPage (ViewCompetenceGridView, viewCompetenceGridPage)
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
  (M.component model update view) {M.subs = [M.uriSub SetURI]}
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

    topMenu =
      V.hBox_
        (V.Expand V.Start)
        V.NoExpand
        V.SmallGap
        [ V.buttonRow
            [ V.link [M.onClick $ PushURI viewCompetenceGridUri] "View"
            , V.link [M.onClick $ PushURI editCompetenceGridUri] "Edit"
            ]
        ]

    page uri =
      case M.route (Proxy @API) (viewCompetenceGrid :<|> editCompetenceGrid) id uri of
        Left _ -> V.text_ "404"
        Right v -> v

    viewCompetenceGrid _ = mounted ViewCompetenceGrid $ viewCompetenceGridPage r u
    editCompetenceGrid _ = mounted EditCompetenceGrid $ editCompetenceGridPage r u

    mounted key c = M.div_ [M.key_ key] M.+> c

    footer = V.hBox_ (V.Expand V.Center) V.NoExpand V.NoGap [V.text_ "© 2025 Markus Mayr"]

withTailwindPlay :: App -> App
withTailwindPlay app = app {M.scripts = M.Src "https://cdn.tailwindcss.com" : M.scripts app}

type API = ("view" :> ViewCompetenceGridView) :<|> EditCompetenceGridView

viewCompetenceGridUri, editCompetenceGridUri :: M.URI
viewCompetenceGridUri :<|> editCompetenceGridUri = allLinks' linkURI (Proxy @API)

data Page
  = ViewCompetenceGrid
  | EditCompetenceGrid
  deriving (Eq, Show)

instance M.ToKey Page where
  toKey = M.toKey . show
