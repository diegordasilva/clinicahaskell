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

formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,) <$>
             areq emailField "E-mail" Nothing <*>
             areq passwordField "Senha" Nothing

getLoginpacR :: Handler Html
getLoginpacR = do
    (widget, enctype) <- generateFormPost formLogin
    defaultLayout $ widgetForm LoginpacR enctype widget "Login"

postLoginpacR :: Handler Html
postLoginpacR = do
            ((result, _), _) <- runFormPost formLogin
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
    (widget, enctype) <- generateFormPost formLogin
    defaultLayout $ widgetForm LoginfuncR enctype widget "Login"

postLoginfuncR :: Handler Html
postLoginfuncR = do
            ((result, _), _) <- runFormPost formLogin
            case result of
                FormSuccess (email, password) -> do
                   cara <- runDB $ selectFirst [FuncionarioEmail ==. email,
                                                FuncionarioSenha ==. password] []
                   case cara of
                       Just (Entity funcid funcionario) -> do
                           setSession "_ID" (pack $ show $ fromSqlKey funcid)
                           redirect (FuncionarioR funcid)
                       Nothing -> redirect LoginfuncR
                _ -> redirect CadastrofuncionarioR

postLogoutR:: Handler Html
postLogoutR = do
    deleteSession "_ID"
    redirect HelloR