{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Login where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius
import Database.Persist.Postgresql

formLoginpac :: Form (Text, Text)
formLoginpac = renderDivs $ (,) <$>
             areq emailField         "E-mail" Nothing <*>
             areq passwordField      "Senha" Nothing

formLoginfunc :: Form (Text, Text)
formLoginfunc = renderDivs $ (,) <$>
             areq emailField         "E-mail" Nothing <*>
             areq passwordField      "Senha" Nothing

getLoginpacR :: Handler Html
getLoginpacR = do
    (widget, enctype) <- generateFormPost formLoginpac
    defaultLayout $ widgetForm LoginpacR enctype widget "Login Paciente"

postLoginpacR :: Handler Html
postLoginpacR = do
            ((result, _), _) <- runFormPost formLoginpac
            case result of
                FormSuccess (email, password) -> do
                   cara <- runDB $ selectFirst [PacienteEmail ==. email,
                                                PacienteSenha ==. password] []
                   case cara of
                       Just (Entity pacid paciente) -> do
                           setSession "_ID" (pack $ show $ fromSqlKey pacid)
                           redirect (PacienteR pacid)
                       Nothing -> redirect LoginpacR
                _ -> redirect CadastropacienteR
                
getLoginfuncR :: Handler Html
getLoginfuncR = do
    (widget, enctype) <- generateFormPost formLoginfunc
    defaultLayout $ widgetForm LoginfuncR enctype widget "Login Funcionario"

postLoginfuncR :: Handler Html
postLoginfuncR = do
            ((result, _), _) <- runFormPost formLoginfunc
            case result of
                FormSuccess (email, password) -> do
                   coroa <- runDB $ selectFirst [FuncionarioEmail ==. email,
                                                FuncionarioSenha ==. password] []
                   case coroa of
                       Just (Entity funcid funcionario) -> do
                           setSession "_ID" (pack $ show $ fromSqlKey funcid)
                           redirect (FuncionarioR funcid)
                       Nothing -> redirect LoginfuncR
                _ -> redirect CadastrofuncionarioR

postLogoutR:: Handler Html
postLogoutR = do
    deleteSession "_ID"
    redirect HelloR