module Competences.Frontend.Component.CompetenceGrid.Types
  ( CompetenceGridMode (..)
  )
where

-- | Mode for the competence grid component
data CompetenceGridMode
  = GridView
  | GridEdit
  | GridResources
  | GridAssessment
  | GridGrading
  deriving (Eq, Ord, Enum, Bounded, Show)
