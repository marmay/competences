module Competences.Frontend.Component.CompetenceGrid.Planning
  ( planningDetailView
  )
where

import Competences.Command (Command (..), MesoPlansCommand (..), MesoPlanEntryPatch (..), EntityCommand (..))
import Competences.Common.IxSet qualified as Ix
import Competences.Document
  ( CompetenceGrid (..)
  , Document (..)
  , Lock (..)
  , Order
  )
import Competences.Document.MesoPlan (MesoPlan (..), MesoPlanEntry (..))
import Competences.Document.Order (orderMax, orderPosition)
import Competences.Frontend.Common qualified as C
import Competences.Frontend.Component.CompetenceGrid.Types (CompetenceGridMode)
import Competences.Frontend.Component.Editor qualified as TE
import Competences.Frontend.Component.Editor.TableView qualified as TE
import Competences.Frontend.Component.Editor.Types (translateReorder')
import Competences.Frontend.Component.SelectorDetail qualified as SD
import Competences.Frontend.SyncContext
  ( DocumentChange (..)
  , SyncContext
  , modifySyncDocument
  , nextId
  , subscribeDocument
  )
import Competences.Frontend.View qualified as V
import Competences.Frontend.View.Button qualified as Button
import Competences.Frontend.View.Icon (Icon (..))
import Competences.Frontend.View.Tailwind (class_)
import Competences.Frontend.View.Typography qualified as Typography
import Data.Map qualified as Map
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Miso qualified as M
import Miso.Html qualified as MH
import Optics.Core ((&), (?~), (.~))

-- ============================================================================
-- PLANNING VIEW
-- ============================================================================

-- | Minimal model - only what's needed beyond Document projection
data PlanningModel = PlanningModel
  { mesoPlan :: !(Maybe MesoPlan)
  , entries :: ![MesoPlanEntry]
  }
  deriving (Eq, Generic, Show)

-- | Actions for the planning component
data PlanningAction
  = DocumentUpdated !DocumentChange
  | CreateMesoPlan
  | CreateNewEntry
  deriving (Eq, Show)

-- | Project from document to minimal model
projectPlanning :: CompetenceGrid -> Document -> PlanningModel
projectPlanning grid doc =
  let mPlan = Ix.getOne (doc.mesoPlans Ix.@= grid.id)
      entries' = case mPlan of
        Nothing -> []
        Just plan -> Ix.toAscList (Proxy @Order) (doc.mesoPlanEntries Ix.@= plan.id)
   in PlanningModel mPlan entries'

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
    initialModel = PlanningModel Nothing []

    update (DocumentUpdated dc) = M.modify $ \_ -> projectPlanning grid dc.document

    update CreateMesoPlan = M.io_ $ do
      planId <- nextId r
      let plan =
            MesoPlan
              { id = planId
              , competenceGridId = grid.id
              , title = grid.title
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

    view m = case m.mesoPlan of
      Nothing -> noPlanView
      Just plan -> planView plan

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

    planView _plan =
      V.viewFlow
        ( V.vFlow
            & (#expandDirection .~ V.Expand V.Start)
            & (#expandOrthogonal .~ V.Expand V.Center)
            & (#gap .~ V.SmallSpace)
        )
        [ V.component
            ("meso-plan-entries-editor-" <> M.ms (show grid.id))
            (TE.editorComponent mesoPlanEntriesEditor r)
        , MH.div_
            [class_ "flex gap-2"]
            [ Button.buttonPrimary (C.translate' C.LblAddMesoPlanEntry)
                & Button.withIcon IcnAdd
                & Button.withClick CreateNewEntry
                & Button.renderButton
            ]
        ]

    mesoPlanEntryEditable =
      TE.editable
        ( \d -> case Ix.getOne (d.mesoPlans Ix.@= grid.id) of
            Nothing -> []
            Just plan ->
              map
                (\e -> (e, d.locks Map.!? MesoPlanEntryLock e.id))
                (Ix.toAscList (Proxy @Order) (d.mesoPlanEntries Ix.@= plan.id))
        )
        & (#modify ?~ (\e m -> MesoPlans $ OnMesoPlanEntries (Modify e.id m)))
        & (#delete ?~ (\e -> MesoPlans $ OnMesoPlanEntries (Delete e.id)))
        & ( #reorder
              ?~ ( \d e a -> do
                     p <- orderPosition d.mesoPlanEntries e.id
                     pure $ MesoPlans $ ReorderMesoPlanEntry p (translateReorder' (.id) a)
                 )
          )

    mesoPlanEntriesEditor =
      TE.editor
        TE.editorTableRowView'
        mesoPlanEntryEditable
        `TE.addNamedField` ( C.translate' C.LblMesoPlanEntryTitle
                           , TE.textEditorField #title #title
                           )
        `TE.addNamedField` ( C.translate' C.LblMesoPlanEntryDescription
                           , TE.richTextEditorField #description #description
                           )
