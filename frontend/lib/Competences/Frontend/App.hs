module Competences.Frontend.App
  ( runApp
  , mkApp
  , withTailwindPlay
  )
where

import Competences.Document.User (User)
import Competences.Frontend.Page.EditCompetenceGridPage (editCompetenceGridPage)
import Competences.Frontend.Page.ManageUsersPage (manageUsersPage)
import Competences.Frontend.Page.ViewCompetenceGridPage (viewCompetenceGridPage)
import Competences.Frontend.SyncDocument (SyncDocumentRef)
import Competences.Frontend.View qualified as V
import Data.Functor (($>))
import GHC.Generics (Generic)
import Language.Javascript.JSaddle (JSM)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.Router qualified as M
import Optics.Core ((.~), (^.))

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
    model = Model $ M.toURI EditCompetenceGrid

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
            [ V.link [M.onClick $ PushURI $ M.toURI ViewCompetenceGrid] "View"
            , V.link [M.onClick $ PushURI $ M.toURI EditCompetenceGrid] "Edit"
            , V.link [M.onClick $ PushURI $ M.toURI ManageUsers] "Users"
            ]
        ]

    page uri = case M.route uri of
      Left _ -> V.text_ "404"
      Right v -> case v of
        ViewCompetenceGrid -> viewCompetenceGrid
        EditCompetenceGrid -> editCompetenceGrid
        ManageUsers -> manageUsers

    viewCompetenceGrid = mounted ViewCompetenceGrid $ viewCompetenceGridPage r u
    editCompetenceGrid = mounted EditCompetenceGrid $ editCompetenceGridPage r u
    manageUsers = mounted ManageUsers $ manageUsersPage r u

    mounted key c = M.div_ [M.key_ key] M.+> c

    footer = V.hBox_ (V.Expand V.Center) V.NoExpand V.NoGap [V.text_ "© 2025 Markus Mayr"]

withTailwindPlay :: App -> App
withTailwindPlay app = app {M.scripts = M.Src "https://cdn.tailwindcss.com" : M.scripts app}

data Page
  = ViewCompetenceGrid
  | EditCompetenceGrid
  | ManageUsers
  deriving (Eq, Show)

instance M.Router Page where
  routeParser =
    M.routes
      [ M.path "view" $> ViewCompetenceGrid
      , M.path "edit" $> EditCompetenceGrid
      , M.path "users" $> ManageUsers
      ]
  fromRoute ViewCompetenceGrid = [M.toPath "view"]
  fromRoute EditCompetenceGrid = [M.toPath "edit"]
  fromRoute ManageUsers = [M.toPath "users"]

instance M.ToKey Page where
  toKey = M.toKey . show
