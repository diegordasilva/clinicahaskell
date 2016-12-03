{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handlers.Agendamento where
import Foundation
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Data.Monoid

formAgendamento :: Form [MedicoId]
formAgendamento = renderDivs $ areq (multiSelectField medicoLista) "Lista de medicos" Nothing
              where
                medicoLista = do
                    medico <- runDB $ selectList [] [Asc MedicoId]
                    optionsPairs $ Prelude.map (\m -> (mconcat [medicoNome $ entityVal m, " - ", medicoEspecialidade $ entityVal m, " - ", pack $ show $ medicoCrm $ entityVal m], entityKey m)) medico


getAgendamentoR :: Handler Html
getAgendamentoR = do
    (widget,enctype) <- generateFormPost formAgendamento
    defaultLayout $ do
        [whamlet|
            <form action=@{AgendamentoR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
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
                        pid <- (return $ read $ unpack pacienteStr) :: Handler PacienteId
                        sequence $ fmap (\mid -> runDB $ insert $ Agendamento pid mid) agendamentos
                        defaultLayout [whamlet| <h1> Agendamento cadastradas com sucesso! |]
            _ -> redirect HelloR