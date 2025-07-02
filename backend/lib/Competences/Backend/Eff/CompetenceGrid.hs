{-# LANGUAGE TypeFamilies #-}
module Competences.Backend.Eff.CompetenceGrid
  ( Storage(..)
  , runStorageMem
  )
where

import Competences.Model.Grid (CompetenceGrid, CompetenceGridId)
import Competences.Model.User (UserId)
import Effectful (Effect, Eff, IOE, DispatchOf, Dispatch(Dynamic), (:>), inject)
import Effectful.Dispatch.Dynamic (interpret_, interpret, reinterpret, EffectHandler)
import qualified Effectful.State.Static.Shared as S
import qualified Data.Map as M

data CompetenceGrid :: Effect where
  ListAllCompetenceGrids :: Storage m [CompetenceGrid]
  ListUserCompetenceGrids :: UserId -> Storage m [CompetenceGrid]
  InsertOrUpdateCompetenceGrid :: CompetenceGrid -> Storage m ()
  DeleteCompetenceGrid :: CompetenceGridId -> Storage m ()

type instance DispatchOf Storage = Dynamic

type StorageState = M.Map CompetenceGridId CompetenceGrid

-- Define the handler for the Storage effect using State
runStorageMem :: forall es a. Eff (Storage ': es) a -> Eff es a
runStorageMem = reinterpret (S.evalState initialState) $ \_ -> \case
    ListAllCompetenceGrids -> S.gets @StorageState M.elems
    ListUserCompetenceGrids userId -> undefined -- S.gets (filter ((== userId) . competenceGridUserId) . M.elems)
    InsertOrUpdateCompetenceGrid grid -> undefined -- S.modify' (M.insert (competenceGridId grid) grid)
    DeleteCompetenceGrid gridId -> undefined -- S.modify' (M.delete gridId)
  where
    initialState :: StorageState
    initialState = M.empty
