module Competences.Frontend.App.Action
  (
  )
  where

data Action
  = Trigger !Command
  | Process !(CommandId, Command)
  | PushModal !(SomeComponent)
  | PopModal
  | OpenSideBar !(SomeComponent)
  | CloseSidebar
  | ChangeGridMode
