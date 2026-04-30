-- | Resource entity selector — a thin config wrapper around
-- 'entitySelectorComponent'.
--
-- Resources have no draft/published distinction (no 'WithOrigin'),
-- so the selected type is plain 'Resource'. The dropdown carries
-- four creation entries (inline / web link / video link / file).
-- The "import resources" entry is intentionally absent here; it is
-- a cross-cutting concern and lives in the main menu (see
-- docs/TODO.md immediate follow-ups).
module Competences.Frontend.Component.Resource.Selector
  ( resourceSelectorComponent
  )
where

import Competences.Command (Command (..), EntityCommand (..), ResourcesCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document (Document (..), Resource (..), ResourceContent (..), ResourceIxs)
import Competences.Document.FileRef (FileRef (..), SHA256Hash (..))
import Competences.Document.Resource (ResourceId, ResourceIdentifier (..), mkResource)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.Selector.Entity
  ( Action (..)
  , CreateAction (..)
  , EntitySelectorConfig (..)
  , ItemRenderer (..)
  , Model
  , entitySelectorComponent
  )
import Competences.Frontend.Component.Selector.UriBinding (pageBinding)
import Competences.Frontend.Fragment.SelectorFilter (searchOnlyFilter)
import Competences.Frontend.Page (Page (..))
import Competences.Frontend.SyncContext (SyncContext, modifySyncDocument, nextId)
import Competences.Frontend.View.Icon qualified as Icon
import Competences.Frontend.View.SelectorList qualified as SL
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Miso qualified as M
import Miso.String (ms)
import Optics.Core (Lens')

-- | The selector's selected type: a plain 'Resource' (no draft variant).
type Selected = Resource

-- | Projection for the generic selector — just the indexed resources.
type Projection = Ix.IxSet ResourceIxs Resource

-- | Mount a resource selector. @parentLens@ points to the
-- @Maybe Resource@ slot in the parent's model that the selector
-- writes to.
resourceSelectorComponent
  :: SyncContext
  -> Maybe ResourceId
  -- ^ Deep-linked resource at first mount, if any.
  -> Lens' p (Maybe Selected)
  -> M.Component p (Model Selected Projection Text) (Action Selected Projection Text)
resourceSelectorComponent r mResourceId parentLens =
  entitySelectorComponent r (config parentLens mResourceId)

config
  :: Lens' p (Maybe Selected)
  -> Maybe ResourceId
  -> EntitySelectorConfig p Selected Projection ResourceIxs ResourceId Text Text
config parentLens mResourceId =
  EntitySelectorConfig
    { title = C.translate' C.LblManageResources
    , project = \doc _user -> doc.resources
    , emptyProjection = Ix.empty
    , entitiesOf = id
    , itemsInOrder = sortOn resourceIdentifierText . Ix.toList
    , idOf = (.id)
    , lookupBy = \xs rid -> Ix.getOne (xs Ix.@= rid)
    , itemView = ItemRenderer renderItem
    , createActions =
        [ mkCreate Icon.IcnResources C.LblInlineContent (InlineContent mempty)
        , mkCreate Icon.IcnLink C.LblWebLink (WebLink "" "")
        , mkCreate Icon.IcnVideo C.LblVideoLink (VideoLink "" "")
        , mkCreate Icon.IcnExport C.LblFile (FileContent (FileRef (SHA256Hash "") "" "" 0))
        ]
    , uriBinding =
        Just $ pageBinding (ManageResources . Just) $ \case
          ManageResources (Just rid) -> Just rid
          _ -> Nothing
    , initialPick = Just $ \xs ->
        case mResourceId of
          Just rid -> case Ix.getOne (xs Ix.@= rid) of
            Just hit -> Just hit
            Nothing -> firstByIdentifier xs
          Nothing -> firstByIdentifier xs
    , filter = searchOnlyFilter (C.translate' C.LblFilterResources) resourceIdentifierText
    , parentLens = parentLens
    }
  where
    mkCreate icn lbl content =
      CreateAction
        { icon = icn
        , label = C.translate' lbl
        , run = \r -> do
            rid <- nextId r
            let res = (mkResource rid){content = content}
            modifySyncDocument r $ Resources (OnResources (CreateAndLock res))
            pure (Just res)
        }

renderItem
  :: Selected
  -> Bool
  -> M.View m (Action Selected Projection Text)
renderItem res isSel =
  let ResourceIdentifier ident = res.identifier
      label = ms (if T.null ident then "(Unbenannt)" else ident)
   in SL.selectorItem isSel (contentIcon res.content) label (Pick res)

firstByIdentifier :: Projection -> Maybe Selected
firstByIdentifier xs = case sortOn resourceIdentifierText (Ix.toList xs) of
  [] -> Nothing
  (x : _) -> Just x

resourceIdentifierText :: Resource -> Text
resourceIdentifierText res =
  let ResourceIdentifier ident = res.identifier in ident

contentIcon :: ResourceContent -> Icon.Icon
contentIcon (InlineContent _) = Icon.IcnResources
contentIcon (WebLink _ _) = Icon.IcnLink
contentIcon (VideoLink _ _) = Icon.IcnVideo
contentIcon (FileContent _) = Icon.IcnResources
