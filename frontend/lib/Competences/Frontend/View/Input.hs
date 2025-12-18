module Competences.Frontend.View.Input
  (
  )
  where

comboBox :: [a] -> (a -> Text) -> (Maybe a -> action) -> M.View model action
comboBox xs toString toAction =
  M.select_ [ M.onInput findValue ] 
          ( M.option_ [M.value_ ""] [M.text_ [C.translate' C.LblPleaseSelectItemShort]]
              : map mkOption m.possibleValues
          )
