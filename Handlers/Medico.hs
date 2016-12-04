{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Medico where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql

formMedico :: Form Medico
formMedico = renderDivs $ Medico <$>
            areq textField "Nome" Nothing <*>
            areq textField "Especialidade" Nothing <*>
            areq intField "CRM" Nothing
            
getCadastromedicoR :: Handler Html
getCadastromedicoR = do
             (widget, enctype) <- generateFormPost formMedico
             defaultLayout $ do 
                -- addStylesheet $ StaticR teste_css
                 widgetForm CadastromedicoR enctype widget "Cadastro de Medicos"

postCadastromedicoR :: Handler Html
postCadastromedicoR = do
                ((result, _), _) <- runFormPost formMedico
                case result of
                    FormSuccess medico -> do
                       runDB $ insert medico
                       defaultLayout [whamlet|
                           <h1> #{medicoNome medico} Inserido com sucesso. 
                       |]
                    _ -> redirect CadastromedicoR
                    
getMedicoR :: MedicoId -> Handler Html
getMedicoR medid = do
             medico <- runDB $ get404 medid 
             defaultLayout [whamlet| 
                 <h1> Seja bem-vindx #{medicoNome medico}
             |]
             
postMedicoR :: MedicoId -> Handler Html
postMedicoR medid = do
     runDB $ delete medid
     redirect ListarmedicoR

getListarmedicoR :: Handler Html
getListarmedicoR = do
             listaM <- runDB $ selectList [] [Asc MedicoNome]
             defaultLayout $ do
                 $(whamletFile "templates/listam.hamlet") 
                -- toWidget $(luciusFile "templates/lista.lucius")