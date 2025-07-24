module Competences.Frontend.App.ComponentRegistry
  ( ModalComponent (..)
  , GridComponent (..)
  , SideBarComponent (..)
  , makeModal
  , makeGrid
  , makeSideBar
  )
where

import Competences.Frontend.Modal.CompetenceEditor (competenceEditor)
import Competences.Model (Model)
import Competences.Model.Competence (Competence)
import Control.Concurrent.STM (TChan)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Miso (SomeComponent (..))

data ModalComponent
  = CompetenceEditor !Competence
  deriving (Eq, Show, Generic)

data GridComponent
  = TodoGrid
  deriving (Eq, Show, Generic)

data SideBarComponent
  = TodoSidebar
  deriving (Eq, Show, Generic)

makeModal :: ModalComponent -> IO (TChan Model) -> IO SomeComponent
makeModal (CompetenceEditor competence) = makeComponent' $ SomeComponent $ competenceEditor competence

makeGrid :: GridComponent -> IO (TChan Model) -> IO SomeComponent
makeGrid _ = undefined

makeSideBar :: SideBarComponent -> IO (TChan Model) -> IO SomeComponent
makeSideBar _ = undefined

makeComponent :: (TChan Model -> SomeComponent) -> IO (TChan Model) -> IO SomeComponent
makeComponent f chan = f <$> chan

makeComponent' :: SomeComponent -> IO (TChan Model) -> IO SomeComponent
makeComponent' f _ = pure f

instance ToJSON ModalComponent

instance FromJSON ModalComponent

instance ToJSON GridComponent

instance FromJSON GridComponent

instance ToJSON SideBarComponent

instance FromJSON SideBarComponent
