module Competences.Frontend.Component.Selector.ResourceSelector
  ( resourceSelectorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), ResourcesCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Resource (..), ResourceContent (..), ResourceIxs)
import Competences.Document.FileRef (FileRef (..), SHA256Hash (..))
import Competences.Document.Resource (ResourceIdentifier (..), mkResource)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.ImportModal qualified as ImportModal
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext (..)
  , isInitialUpdate
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.Layout qualified as Layout
import Competences.Frontend.View.SelectorList qualified as SL
import Competences.Frontend.View.Tailwind (class_)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as M
import Miso.String (ms)
import Optics.Core (Lens', toLensVL, (&), (.~), (?~))

data Model = Model
  { allResources :: !(Ix.IxSet ResourceIxs Resource)
  , selectedItem :: !(Maybe Resource)
  , newItem :: !(Maybe Resource)
  , dropdownOpen :: !Bool
  , searchQuery :: !Text
  }
  deriving (Eq, Generic, Show)

data Action
  = SelectItem !Resource
  | CreateInlineResource
  | CreateWebLinkResource
  | CreateVideoLinkResource
  | CreateFileResource
  | ToggleDropdown
  | CloseDropdown
  | SetSearchQuery !Text
  | UpdateDocument !DocumentChange
  | OpenImportModal
  deriving (Eq, Show)

resourceSelectorComponent
  :: SyncContext
  -> Maybe (Ix.IxSet ResourceIxs Resource -> Maybe Resource)
  -> Maybe (Resource -> IO ())
  -> Lens' p (Maybe Resource)
  -> M.Component p Model Action
resourceSelectorComponent r initialSelection onSelect parentLens =
  (M.component model update view')
    { M.bindings = [toLensVL parentLens M.<--- toLensVL #selectedItem]
    , M.subs = [subscribeDocument r UpdateDocument]
    }
  where
    model = Model Ix.empty Nothing Nothing False ""

    update (SelectItem item) = do
      M.modify $ \m ->
        case Ix.getOne (m.allResources Ix.@= item.id) of
          Just res -> m & (#selectedItem ?~ res) & (#newItem .~ Nothing)
          Nothing -> m & (#newItem ?~ item)
      case onSelect of
        Just f -> M.io_ (f item)
        Nothing -> pure ()

    update CreateInlineResource = createResource (InlineContent mempty)
    update CreateWebLinkResource = createResource (WebLink "" "")
    update CreateVideoLinkResource = createResource (VideoLink "" "")
    update CreateFileResource = createResource (FileContent (FileRef (SHA256Hash "") "" "" 0))

    update ToggleDropdown = M.modify $ \m ->
      m & #dropdownOpen .~ not m.dropdownOpen

    update CloseDropdown = M.modify $ \m ->
      m & #dropdownOpen .~ False

    update (SetSearchQuery q) = M.modify $ \m ->
      m & #searchQuery .~ q

    update (UpdateDocument dc) = M.modify $ \m ->
      let allResources' = dc.document.resources
          validatedSelected = case m.selectedItem of
            Just res -> Ix.getOne (allResources' Ix.@= res.id)
            Nothing -> Nothing
          validatedNew = case m.newItem of
            Just res ->
              case Ix.getOne (allResources' Ix.@= res.id) of
                Just res' -> Just res'
                Nothing -> m.newItem
            Nothing -> Nothing
          m' = m
            { allResources = allResources'
            , selectedItem = validatedSelected
            , newItem = validatedNew
            }
       in case (isInitialUpdate dc.change, m'.selectedItem, initialSelection) of
            (True, Nothing, Just f) -> m' {selectedItem = f allResources'}
            _ -> m'

    update OpenImportModal = do
      M.modify $ #dropdownOpen .~ False
      M.io_ $ ImportModal.openImportModal r

    createResource content = M.withSink $ \s -> do
      resourceId <- nextId r
      let newResource = (mkResource resourceId) {content = content}
      modifySyncDocument r $ Resources (OnResources (CreateAndLock newResource))
      s CloseDropdown
      s (SelectItem newResource)

    view' m =
      M.div_
        [class_ "h-full"]
        [ Layout.vFlow
            (Layout.gapS <> Layout.hFull)
            [ SL.selectorHeaderWithDropdown
                (C.translate' C.LblManageResources)
                m.dropdownOpen
                ToggleDropdown
                [ SL.dropdownItem Icon.IcnResources (C.translate' C.LblInlineContent) CreateInlineResource
                , SL.dropdownItem Icon.IcnLink (C.translate' C.LblWebLink) CreateWebLinkResource
                , SL.dropdownItem Icon.IcnVideo (C.translate' C.LblVideoLink) CreateVideoLinkResource
                , SL.dropdownItem Icon.IcnExport (C.translate' C.LblFile) CreateFileResource
                , SL.dropdownItem Icon.IcnImport (C.translate' C.LblImportResources) OpenImportModal
                ]
            , SL.selectorSearchField (ms m.searchQuery) (C.translate' C.LblFilterResources) (SetSearchQuery . M.fromMisoString)
            , viewItems m
            ]
        ]

    viewItems m =
      let allItems = sortOn resourceIdentifierText $ Ix.toList m.allResources
          query = T.toLower m.searchQuery
          filteredItems =
            if T.null query
              then allItems
              else filter (\res -> query `T.isInfixOf` T.toLower (resourceIdentifierText res)) allItems
       in SL.selectorList (map (viewItem m) filteredItems)

    viewItem m res =
      let isSelected = m.selectedItem == Just res || m.newItem == Just res
          icn = contentIcon res.content
          ResourceIdentifier ident = res.identifier
          label = ms $ if T.null ident then "(Unbenannt)" else ident
       in SL.selectorItem isSelected icn label (SelectItem res)

resourceIdentifierText :: Resource -> Text
resourceIdentifierText res =
  let ResourceIdentifier ident = res.identifier in ident

contentIcon :: ResourceContent -> Icon.Icon
contentIcon (InlineContent _) = Icon.IcnResources
contentIcon (WebLink _ _) = Icon.IcnLink
contentIcon (VideoLink _ _) = Icon.IcnVideo
contentIcon (FileContent _) = Icon.IcnResources
