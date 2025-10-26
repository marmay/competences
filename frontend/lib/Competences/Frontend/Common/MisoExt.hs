module Competences.Frontend.Common.MisoExt
  ( onClick'
  ) where

import qualified Miso as M

onClick' :: a -> M.Attribute a
onClick' a = M.onWithOptions options "click" M.emptyDecoder $ \() _ -> a
  where options = M.defaultOptions { M._stopPropagation = True }
