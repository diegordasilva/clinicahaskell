{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell, ViewPatterns #-}
 
module Application where

import Yesod
import Foundation
import Handlers.Agendamento
import Handlers.Login
import Handlers.Medico
import Handlers.Paciente

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" resourcesSitio

getHelloR :: Handler Html
getHelloR = defaultLayout $ do
     sess <- lookupSession "_ID"
     [whamlet|
         <h1> teste
     |]
