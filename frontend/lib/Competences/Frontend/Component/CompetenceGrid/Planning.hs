module Competences.Frontend.Component.CompetenceGrid.Planning
  ( planningDetailView
  )
where

import Competences.Command (Command (..), MesoPlansCommand (..), EntityCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( CompetenceGrid (..)
  , Document (..)
  , Order
  )
import Competences.Document.MesoPlan (MesoPlan (..), MesoPlanEntry (..), MesoPlanEntryId)
import Competences.Document.Order (orderMax)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.CompetenceGrid.EntryEditorModal (entryEditorModal)
import Competences.Frontend.Component.CompetenceGrid.MesoPlanEditorModal (mesoPlanEditorModal)
import Competences.Frontend.Component.CompetenceGrid.Types (CompetenceGridMode)
import Competences.Frontend.Component.CompetenceGrid.LessonPlanEditor (lessonPlanEditorView)
import Competences.Frontend.Component.TaskContentView (renderRichText)
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext (..)
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.SyncContext.ModalManager (openModal)
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core ((&), (.~))

-- ============================================================================
-- PLANNING VIEW
-- ============================================================================

-- | Minimal model - only what's needed beyond Document projection
data PlanningModel = PlanningModel
  { mesoPlan :: !(Maybe MesoPlan)
  , entries :: ![MesoPlanEntry]
  , expandedEntryId :: !(Maybe MesoPlanEntryId)
  }
  deriving (Eq, Generic, Show)

-- | Actions for the planning component
data PlanningAction
  = DocumentUpdated !DocumentChange
  | CreateMesoPlan
  | CreateNewEntry
  | ToggleEntryExpansion !MesoPlanEntryId
  | OpenEntryEditorModal !MesoPlanEntry
  | OpenMesoPlanEditorModal !MesoPlan
  | DeleteEntry !MesoPlanEntryId
  deriving (Eq, Show)

-- | Project from document to minimal model, preserving UI state
projectPlanning :: CompetenceGrid -> Maybe MesoPlanEntryId -> Document -> PlanningModel
projectPlanning grid prevExpanded doc =
  let mPlan = Ix.getOne (doc.mesoPlans Ix.@= grid.id)
      entries' = case mPlan of
        Nothing -> []
        Just plan -> Ix.toAscList (Proxy @Order) (doc.mesoPlanEntries Ix.@= plan.id)
      -- Clear expansion if the entry no longer exists
      expanded = case prevExpanded of
        Nothing -> Nothing
        Just eid -> if any (\e -> e.id == eid) entries' then Just eid else Nothing
   in PlanningModel mPlan entries' expanded

-- | View for planning - allows editing meso plan and entries
planningDetailView
  :: SyncContext
  -> CompetenceGrid
  -> M.View (SD.Model CompetenceGrid CompetenceGridMode) (SD.Action CompetenceGridMode)
planningDetailView r grid =
  V.component
    ("competence-grid-planning-" <> M.ms (show grid.id))
    (planningComponent r grid)

planningComponent :: SyncContext -> CompetenceGrid -> M.Component p PlanningModel PlanningAction
planningComponent r grid =
  (M.component initialModel update view)
    { M.subs = [subscribeDocument r DocumentUpdated]
    }
  where
    initialModel = PlanningModel Nothing [] Nothing

    update (DocumentUpdated dc) = M.modify $ \m -> projectPlanning grid m.expandedEntryId dc.document

    update CreateMesoPlan = M.io_ $ do
      planId <- nextId r
      let plan =
            MesoPlan
              { id = planId
              , competenceGridId = grid.id
              , title = grid.title
              , dateFrom = Nothing
              , dateTo = Nothing
              }
      modifySyncDocument r (MesoPlans $ OnMesoPlans $ CreateAndLock plan)

    update CreateNewEntry = do
      m <- M.get
      M.io_ $ case m.mesoPlan of
        Nothing -> pure ()
        Just plan -> do
          entryId <- nextId r
          let entry =
                MesoPlanEntry
                  { id = entryId
                  , mesoPlanId = plan.id
                  , order = orderMax
                  , title = ""
                  , description = ""
                  , competenceLevels = []
                  }
          modifySyncDocument r (MesoPlans $ OnMesoPlanEntries $ CreateAndLock entry)
          -- Auto-open entry editor modal for immediate configuration
          openModal r.modalManager (entryEditorModal r r.modalManager entry)

    update (ToggleEntryExpansion entryId) = M.modify $ \m ->
      if m.expandedEntryId == Just entryId
        then m & #expandedEntryId .~ Nothing
        else m & #expandedEntryId .~ Just entryId

    update (OpenEntryEditorModal entry) = M.io_ $
      openModal r.modalManager (entryEditorModal r r.modalManager entry)

    update (OpenMesoPlanEditorModal plan) = M.io_ $
      openModal r.modalManager (mesoPlanEditorModal r r.modalManager plan)

    update (DeleteEntry entryId) = M.io_ $
      modifySyncDocument r (MesoPlans $ OnMesoPlanEntries $ Delete entryId)

    view m = case m.mesoPlan of
      Nothing -> noPlanView
      Just plan -> planView plan m

    noPlanView =
      V.viewFlow
        ( V.vFlow
            & (#expandDirection .~ V.Expand V.Start)
            & (#expandOrthogonal .~ V.Expand V.Center)
            & (#gap .~ V.MediumSpace)
        )
        [ MH.div_
            [class_ "flex flex-col items-center gap-4 p-8"]
            [ Typography.muted (C.translate' C.LblNoMesoPlanEntries)
            , Button.buttonPrimary (C.translate' C.LblCreateMesoPlan)
                & Button.withIcon IcnAdd
                & Button.withClick CreateMesoPlan
                & Button.renderButton
            ]
        ]

    planView plan m =
      V.viewFlow
        ( V.vFlow
            & (#expandDirection .~ V.Expand V.Start)
            & (#expandOrthogonal .~ V.Expand V.Center)
            & (#gap .~ V.SmallSpace)
        )
        [ -- Plan header with title, dates, and edit button
          MH.div_
            [class_ "flex items-center justify-between p-3 bg-muted/30 rounded-lg mb-2"]
            [ MH.div_
                [class_ "flex items-center gap-4"]
                [ MH.div_
                    [class_ "font-medium"]
                    [M.text $ M.ms plan.title]
                , -- Date range display
                  case (plan.dateFrom, plan.dateTo) of
                    (Nothing, Nothing) -> M.text ""
                    (Just from, Nothing) ->
                      MH.span_ [class_ "text-sm text-muted-foreground"]
                        [M.text $ C.translate' C.LblMesoPlanDateFrom <> ": " <> C.formatDay from]
                    (Nothing, Just to) ->
                      MH.span_ [class_ "text-sm text-muted-foreground"]
                        [M.text $ C.translate' C.LblMesoPlanDateTo <> ": " <> C.formatDay to]
                    (Just from, Just to) ->
                      MH.span_ [class_ "text-sm text-muted-foreground"]
                        [M.text $ C.formatDay from <> " – " <> C.formatDay to]
                ]
            , Button.buttonGhost ""
                & Button.withIcon IcnEdit
                & Button.withSize Button.Small
                & Button.withClick (OpenMesoPlanEditorModal plan)
                & Button.renderButton
            ]
        , MH.div_
            [class_ "flex flex-col gap-2 w-full"]
            (map (viewEntry m) m.entries)
        , MH.div_
            [class_ "flex gap-2"]
            [ Button.buttonPrimary (C.translate' C.LblAddMesoPlanEntry)
                & Button.withIcon IcnAdd
                & Button.withClick CreateNewEntry
                & Button.renderButton
            ]
        ]

    viewEntry m entry =
      let isExpanded = m.expandedEntryId == Just entry.id
          chevronClass = if isExpanded then "rotate-90" else ""
       in MH.div_
            [class_ "border border-border rounded-lg overflow-hidden"]
            [ -- Entry header
              MH.div_
                [class_ "flex items-center gap-3 p-3 bg-muted/50"]
                [ -- Chevron and content (clickable to expand)
                  MH.div_
                    [ class_ "flex items-center gap-3 flex-1 cursor-pointer hover:bg-muted -m-3 p-3"
                    , MH.onClick (ToggleEntryExpansion entry.id)
                    ]
                    [ -- Chevron icon
                      MH.span_
                        [class_ $ "transition-transform duration-200 " <> chevronClass]
                        [M.text "▶"]
                    , -- Entry title and description
                      MH.div_
                        [class_ "flex-1"]
                        [ MH.div_
                            [class_ "font-medium"]
                            [M.text $ M.ms $ if Text.null entry.title then "(Untitled)" else entry.title]
                        , if Text.null entry.description
                            then M.text ""
                            else MH.div_
                                   [class_ "text-sm text-muted-foreground"]
                                   [renderRichText entry.description]
                        ]
                    ]
                , -- Edit and delete buttons
                  MH.div_
                    [class_ "flex gap-1"]
                    [ Button.buttonGhost ""
                        & Button.withIcon IcnEdit
                        & Button.withSize Button.Small
                        & Button.withClick (OpenEntryEditorModal entry)
                        & Button.renderButton
                    , Button.buttonGhost ""
                        & Button.withIcon IcnDelete
                        & Button.withSize Button.Small
                        & Button.withClick (DeleteEntry entry.id)
                        & Button.renderButton
                    ]
                ]
            , -- Expanded content (LessonPlan view)
              if isExpanded
                then viewExpandedEntry entry
                else M.text ""
            ]

    viewExpandedEntry entry =
      MH.div_
        [class_ "p-4 border-t border-border bg-background"]
        [ -- LessonPlan content (notes + phases)
          lessonPlanEditorView r entry.id
        ]
