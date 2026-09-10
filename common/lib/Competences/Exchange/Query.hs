-- | Provides information about an ExchangeDoc
module Competences.Exchange.Query
  ( exchangeDocAttachments
  )
where

import Competences.Exchange.Types
  ( ExchangeAttachment
  , ExchangeCompetence (..)
  , ExchangeCompetenceGrid (..)
  , ExchangeCompetenceLevelExample (..)
  , ExchangeDoc (..)
  , ExchangeResource (..)
  , ExchangeResourceContent (..)
  , ExchangeTask (..)
  )
import Data.Map qualified as M

exchangeDocAttachments :: ExchangeDoc -> [ExchangeAttachment]
exchangeDocAttachments e =
  concatMap exchangeCompetenceGridAttachments e.competenceGrids
    <> concatMap (.attachments) e.tasks
    <> concatMap (.attachments) e.draftTasks
    <> concatMap exchangeResourceAttachments e.resources

exchangeCompetenceGridAttachments :: ExchangeCompetenceGrid -> [ExchangeAttachment]
exchangeCompetenceGridAttachments =
  concatMap (.attachments)
    . concatMap (concat . M.elems . (.examples))
    . (.competences)

exchangeResourceAttachments :: ExchangeResource -> [ExchangeAttachment]
exchangeResourceAttachments r =
  r.attachments <> exchangeResourceContentAttachments r.content

exchangeResourceContentAttachments :: ExchangeResourceContent -> [ExchangeAttachment]
exchangeResourceContentAttachments (ExFileContent a) = [a]
exchangeResourceContentAttachments _ = []
