{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE QuasiQuotes       #-}

module Handlers.Agendamento where

import Foundation
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Data.Monoid
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Time

formAgendamento :: Form [MedicoId]
formAgendamento = renderDivs $ areq (multiSelectField medicoLista) "Lista de medicos" Nothing
              where
                medicoLista = do
                    entidades <- runDB $ selectList [] [Asc MedicoNome]
                    optionsPairs $ Prelude.map (\m -> (mconcat [medicoNome $ entityVal m, " - ", medicoEspecialidade $ entityVal m, " - ", pack $ show $ medicoCrm $ entityVal m], entityKey m)) entidades


--agendamento = do
       --entidades <- runDB $ selectList [] [Asc MedicoNome] 
       --optionsPairs $ fmap (\ent -> (departamentoSigla $ entityVal ent, entityKey ent)) entidades-}

getAgendamentoR :: Handler Html
getAgendamentoR = do
    (widget,enctype) <- generateFormPost formAgendamento
    defaultLayout $ do
        [whamlet|
            <form action=@{AgendamentoR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Agendar">
        |]

postAgendamentoR :: Handler Html
postAgendamentoR = do
        ((result,_),_)<- runFormPost formAgendamento
        case result of
            FormSuccess agendamentos -> do
                paId <- lookupSession "_ID"
                case paId of
                    Nothing -> redirect HelloR
                    Just paStr -> do
                        pid <- (return $ read $ unpack paStr) :: Handler PacienteId
                        sequence $ fmap (\mid -> runDB $ insert $ Agendamento pid mid) agendamentos
                        defaultLayout [whamlet| <h1> Agendamento cadastrado com sucesso! |]
            _ -> redirect HelloR
            
        