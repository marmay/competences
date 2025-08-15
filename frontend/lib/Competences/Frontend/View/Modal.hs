module Competences.Frontend.View.Modal
  ( modalHost
  , maybeModalHost
  , modalDialog
  )
where

import Competences.Frontend.View.Tailwind qualified as T
import Miso qualified as M

modalHost :: [M.Attribute a] -> [M.View m a] -> M.View m a
modalHost attrs = M.div_ (T.tailwind [ T.ModalHost ] : attrs)

maybeModalHost :: (Eq child) => Maybe (M.Component model child action') -> M.View model action
maybeModalHost (Just c) = M.div_ [T.tailwind [ T.ModalHost ]] M.+> c
maybeModalHost Nothing = M.div_ [] []

modalDialog :: [M.Attribute a] -> [M.View m a] -> M.View m a
modalDialog attrs = M.div_ (T.tailwind [ T.ModalDialog ] : attrs)
