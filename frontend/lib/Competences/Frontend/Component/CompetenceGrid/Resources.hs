module Competences.Frontend.Component.CompetenceGrid.Resources
  ( resourcesDetailView
  )
where

import Competences.Command (Command (..), ResourcesCommand (..))
import Competences.Command.Common qualified as EC
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( Competence (..)
  , CompetenceGrid (..)
  , Document (..)
  , Level (..)
  , LevelInfo (..)
  , Order
  , Resource (..)
  , ResourceContent (..)
  , ResourceIdentifier (..)
  )
import Competences.Document.Competence (CompetenceLevelId)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.CompetenceGrid.Types (CompetenceGridMode)
import Competences.Frontend.Component.Resource.EditorDetail qualified as ResourceEditor
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext
  ( ProjectedChange (..)
  , SyncContext
  , modifySyncDocument
  , nextId
  , subscribeWithProjection
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon (Icon (..), icon)
import Competences.Frontend.View.Tailwind (class_)
import Data.Function ((&))
import Data.List (find)
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MP

-- ============================================================================
-- RESOURCES MODE DETAIL
-- ============================================================================

-- | A competence level with its parent competence info for display
data CompetenceLevelInfo = CompetenceLevelInfo
  { competence :: !Competence
  , level :: !Level
  , levelInfo :: !LevelInfo
  }
  deriving (Eq, Generic, Show)

-- | Projection data for resources
data ResourceProjection = ResourceProjection
  { resources :: ![Resource]
  , competenceLevels :: ![CompetenceLevelInfo]
  }
  deriving (Eq, Generic, Show)

-- | Model for the resources detail component
data ResourcesModel = ResourcesModel
  { editingResource :: !(Maybe Resource)
  , gridResources :: ![Resource]
  , gridCompetenceLevels :: ![CompetenceLevelInfo]
  }
  deriving (Eq, Generic, Show)

-- | Action for the resources detail component
data ResourcesAction
  = CreateResourceForLevel !CompetenceLevelId
  | EditResource !Resource
  | ClearEditingResource
  | ResourcesUpdated !(ProjectedChange ResourceProjection)
  deriving (Eq, Generic, Show)

-- | View for the resources detail - allows managing resources for a competence grid
resourcesDetailView
  :: SyncContext
  -> CompetenceGrid
  -> M.View (SD.Model CompetenceGrid CompetenceGridMode) (SD.Action CompetenceGridMode)
resourcesDetailView r grid =
  V.component
    ("competence-grid-resources-" <> M.ms (show grid.id))
    (resourcesComponent r grid)

resourcesComponent :: SyncContext -> CompetenceGrid -> M.Component p ResourcesModel ResourcesAction
resourcesComponent r grid =
  (M.component initialModel update view)
    { M.subs = [subscribeWithProjection r resourceProjection ResourcesUpdated]
    }
  where
    initialModel =
      ResourcesModel
        { editingResource = Nothing
        , gridResources = []
        , gridCompetenceLevels = []
        }

    -- Projection to get resources and flattened competence levels for this grid
    resourceProjection doc _ =
      let comps = Ix.toAscList (Proxy @Order) $ doc.competences Ix.@= grid.id
          compLvls = concatMap flattenCompetence comps
          lvlIds = map (\cli -> (cli.competence.id, cli.level)) compLvls
       in ResourceProjection
            { resources = Ix.toList $ doc.resources Ix.@+ lvlIds
            , competenceLevels = compLvls
            }

    flattenCompetence comp =
      [ CompetenceLevelInfo comp lvl info
      | (lvl, info) <- Map.toList comp.levels
      ]

    update (CreateResourceForLevel levelId) = M.io_ $ do
      resourceId <- nextId r
      let resource =
            Resource
              { id = resourceId
              , identifier = ResourceIdentifier ""
              , competenceLevels = [levelId]
              , content = InlineContent ""
              }
      modifySyncDocument r (Resources $ OnResources $ EC.CreateAndLock resource)

    update (EditResource res) = M.modify $ \m -> m {editingResource = Just res}

    update ClearEditingResource = M.modify $ \m -> m {editingResource = Nothing}

    update (ResourcesUpdated change) = M.modify $ \m ->
      m
        { gridResources = change.projection.resources
        , gridCompetenceLevels = change.projection.competenceLevels
        }

    view m =
      MH.div_ [class_ "space-y-4"]
        [ case m.editingResource of
            Nothing -> resourceListView m.gridCompetenceLevels m.gridResources
            Just res ->
              let -- Find competence level info for the first competence level of this resource
                  mCompLevelInfo = case res.competenceLevels of
                    [] -> Nothing
                    (levelId : _) -> find (\cli -> (cli.competence.id, cli.level) == levelId) m.gridCompetenceLevels
               in MH.div_ []
                    [ -- Header with back button and competence level info
                      MH.div_ [class_ "flex items-center gap-2 mb-4 border-b border-stone-200 pb-4"]
                        [ Button.buttonGhost "← Zurück"
                            & Button.withClick ClearEditingResource
                            & Button.withSize Button.Small
                            & Button.renderButton
                        , case mCompLevelInfo of
                            Nothing -> V.empty
                            Just cli ->
                              MH.div_ [class_ "flex-1 min-w-0"]
                                [ MH.div_ [class_ "font-medium text-stone-700 truncate"]
                                    [M.text $ M.ms cli.competence.description]
                                , MH.div_ [class_ "text-sm text-stone-500 truncate"]
                                    [ M.text $ C.translate' (C.LblCompetenceLevelDescription cli.level)
                                        <> ": " <> M.ms cli.levelInfo.description
                                    ]
                                ]
                        ]
                    , ResourceEditor.resourceInlineEditor r res
                    ]
        ]

-- | View for listing resources grouped by competence level with add buttons
resourceListView :: [CompetenceLevelInfo] -> [Resource] -> M.View ResourcesModel ResourcesAction
resourceListView compLevels resources =
  if null compLevels
    then
      MH.div_
        [class_ "text-center py-8 text-stone-500"]
        [MH.p_ [] [M.text "Keine Kompetenzen definiert"]]
    else MH.div_ [class_ "space-y-4"] (map levelSection compLevels)
  where
    levelSection cli =
      let levelId = (cli.competence.id, cli.level)
          levelResources = filter (\res -> levelId `elem` res.competenceLevels) resources
       in MH.div_
            [class_ "border border-stone-200 rounded-lg overflow-hidden"]
            [ -- Header row 1: Competence description + Add button
              MH.div_
                [class_ "flex items-center justify-between px-3 py-2 bg-stone-50 border-b border-stone-200"]
                [ MH.span_
                    [class_ "font-medium text-stone-700 truncate flex-1 mr-2"]
                    [M.text $ M.ms cli.competence.description]
                , Button.buttonGhost (C.translate' C.LblAddResource)
                    & Button.withIcon IcnAdd
                    & Button.withSize Button.Small
                    & Button.withClick (CreateResourceForLevel levelId)
                    & Button.renderButton
                ]
            , -- Header row 2: Level name + level description
              MH.div_
                [class_ "flex items-center gap-2 px-3 py-1.5 bg-stone-100/50 border-b border-stone-200 text-sm min-w-0"]
                [ MH.span_
                    [class_ "font-medium text-stone-600 flex-shrink-0"]
                    [M.text $ C.translate' (C.LblCompetenceLevelDescription cli.level) <> ":"]
                , MH.span_
                    [class_ "text-stone-500 truncate"]
                    [M.text $ M.ms cli.levelInfo.description]
                ]
            , -- Resources for this level
              if null levelResources
                then
                  MH.div_
                    [class_ "px-3 py-4 text-center text-stone-400 text-sm"]
                    [M.text $ C.translate' C.LblNoResources]
                else MH.div_ [class_ "divide-y divide-stone-100"] (map resourceRow levelResources)
            ]

    resourceRow res =
      MH.div_
        [ class_ "flex items-center gap-3 px-3 py-2 hover:bg-stone-50 cursor-pointer"
        , MH.onClick (EditResource res)
        ]
        [ icon [class_ "text-stone-400 flex-shrink-0", MP.width_ "16", MP.height_ "16"] IcnResources
        , MH.div_
            [class_ "flex-1 min-w-0"]
            [ MH.span_
                [class_ "text-sm text-stone-900 truncate"]
                [let ResourceIdentifier ident = res.identifier in M.text (M.ms $ if T.null ident then "(Unbenannt)" else ident)]
            ]
        , MH.span_ [class_ "text-xs text-stone-400"] [M.text $ contentSummary res.content]
        ]

    contentSummary (InlineContent t) = if T.null t then "Inline" else M.ms (T.take 30 t <> if T.length t > 30 then "..." else "")
    contentSummary (WebLink _ _) = "Web-Link"
    contentSummary (VideoLink _ _) = "Video"
