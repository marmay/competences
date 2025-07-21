module Competences.Frontend.App.State
  (
  )
  where

data UI = UI
  { modal :: !(Maybe SomeComponent)
  , grid :: !(Maybe SomeComponent)
  , sidebar :: !(Maybe SomeComponent)
  }

data State = State
  {
  }
