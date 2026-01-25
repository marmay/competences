{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Competences.Frontend.Component.ModalHost
-- Description : Modal host component that subscribes to ModalManager
--
-- Renders modals from the central ModalManager. Mount once in App.hs.
module Competences.Frontend.Component.ModalHost
  ( modalHostComponent
  )
where

import Competences.Frontend.SyncContext.ModalManager
  ( AnyModal (..)
  , Model (..)
  , ModalChange (..)
  , ModalManagerRef
  , closeModal
  , subscribeModals
  )
import Competences.Frontend.View.Tailwind (class_)
import Miso qualified as M
import Miso.Html qualified as MH
import Miso.Html.Event (onClick)

-- | Actions for ModalHost
data Action
  = ModalChanged !ModalChange
  | BackdropClicked

-- | The ModalHost component subscribes to ModalManager and renders modals
modalHostComponent :: ModalManagerRef -> M.Component p Model Action
modalHostComponent ref =
  (M.component model update view)
    { M.subs = [subscribeModals ref ModalChanged]
    }
  where
    model = Model {activeModal = Nothing}

    update (ModalChanged change) =
      M.modify $ \m -> m {activeModal = change.modal}

    update BackdropClicked =
      M.io_ $ closeModal ref

    view m = case m.activeModal of
      Nothing -> M.text ""
      Just (AnyModal comp) ->
        MH.div_
          [ class_ "fixed inset-0 z-50 flex items-center justify-center"
          ]
          [ -- Backdrop
            MH.div_
              [ class_ "absolute inset-0 bg-foreground/50"
              , onClick BackdropClicked
              ]
              []
          , -- Modal content (mounted as child component)
            MH.div_
              [class_ "relative z-10"]
              ["modal-content" M.+> comp]
          ]
