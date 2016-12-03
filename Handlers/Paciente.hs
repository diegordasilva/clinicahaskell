{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Paciente where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius
import Database.Persist.Postgresql
import Database.Persist

formPaciente :: Form Paciente
formPaciente = renderDivs $ Paciente <$>
             areq textField "Nome" Nothing <*>
             areq intField "Telefone" Nothing <*>
             areq intField "CPF" Nothing <*>
             areq textField "Endereço" Nothing <*>
             areq emailField "E-mail" Nothing <*>
             areq passwordField "Senha" Nothing

{-agendamento = do
       entidades <- runDB $ selectList [] [Asc MedicoNome] 
       optionsPairs $ fmap (\ent -> (departamentoSigla $ entityVal ent, entityKey ent)) entidades-}

getCadastropacienteR :: Handler Html
getCadastropacienteR = do
             (widget, enctype) <- generateFormPost formPaciente
             defaultLayout $ do 
                -- addStylesheet $ StaticR teste_css
                 widgetForm CadastropacienteR enctype widget "Cadastro de Pacientes"

postCadastropacienteR :: Handler Html
postCadastropacienteR = do
                ((result, _), _) <- runFormPost formPaciente
                case result of
                    FormSuccess paciente -> do
                       unicoEmail <- runDB $ getBy $ UniqueEmail (pacienteEmail paciente)
                       case unicoEmail of
                           Just _ -> redirect CadastropacienteR
                           Nothing -> do 
                              pacid <- runDB $ insert paciente 
                              redirect (PacienteR pacid)
                    _ -> redirect CadastropacienteR

getPacienteR :: PacienteId -> Handler Html
getPacienteR pid = do
             paciente <- runDB $ get404 pid 
             defaultLayout [whamlet| 
                 <h1> Seja bem-vindx #{pacienteNome paciente}
             |]
             
postPacienteR :: PacienteId -> Handler Html
postPacienteR pid = do
     runDB $ delete pid
     redirect ListarpacienteR

getListarpacienteR :: Handler Html
getListarpacienteR = do
             listaP <- runDB $ selectList [] [Asc PacienteNome]
             defaultLayout $ do
                 $(whamletFile "templates/lista.hamlet") 
                 toWidget $(luciusFile "templates/lista.lucius")

