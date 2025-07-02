{-# LANGUAGE OverloadedStrings #-}

module MyLib (app) where

import Miso

app :: JSM ()
app = startApp app'
  
app' :: App () ()
app' = App
  { model = ()
  , update = \_ _ -> pure ()
  , view = \_ -> div_ [] [ text "Hello, World!!" ]
  , subs = []
  , events = defaultEvents
  , initialAction = ()
  , mountPoint = Nothing
  , logLevel = Off
  }
