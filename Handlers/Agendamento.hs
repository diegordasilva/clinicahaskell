{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module Handlers.Agendamento where

import Foundation
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Data.Monoid
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Time.Calendar

formAgendamento :: Form [MedicoId]
formAgendamento = renderDivs $ areq (multiSelectField medicoLista) "Lista de medicos" Nothing
              where
                medicoLista = do
                    entidade <- runDB $ selectList [] [Asc MedicoNome]
                    --optionsPairs $ Prelude.map (\v -> (mconcat [vooOrigem $ entityVal v, " - ", vooDestino $ entityVal v, " - ", pack $ show $ vooPreco $ entityVal v, " - ", pack $ show $ vooEmbarque $ entityVal v], entityKey v)) voos
                    optionsPairs $ Prelude.map (\m -> (mconcat [medicoNome $ entityVal m, " - ", medicoEspecialidade $ entityVal m, " - ", pack $ show $ medicoCrm $ entityVal m], entityKey m)) entidade


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
                pacienteId <- lookupSession "_ID"
                case pacienteId of
                    Nothing -> redirect HelloR
                    Just pacienteStr -> do
                        pacid <- (return $ read $ unpack pacienteStr) :: Handler PacienteId
                        sequence $ fmap (\medid -> runDB $ insert $ Agendamento pacid medid) agendamentos
                        defaultLayout [whamlet| <h1> Agendamento cadastrado com sucesso! |]
            _ -> redirect HelloR