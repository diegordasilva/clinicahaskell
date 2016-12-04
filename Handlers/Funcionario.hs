{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers.Funcionario where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius
import Database.Persist.Postgresql
import Database.Persist

formFuncionario :: Form Funcionario
formFuncionario = renderDivs $ Funcionario <$>
             areq textField "Nome" Nothing <*>
             areq intField "Telefone" Nothing <*>
             areq intField "CPF" Nothing <*>
             areq textField "Endereço" Nothing <*>
             areq emailField "E-mail" Nothing <*>
             areq passwordField "Senha" Nothing
             
             
getCadastrofuncionarioR :: Handler Html
getCadastrofuncionarioR = do
             (widget, enctype) <- generateFormPost formFuncionario
             defaultLayout $ do 
                -- addStylesheet $ StaticR teste_css
                 widgetForm CadastrofuncionarioR enctype widget "Cadastro de Funcionarios"

postCadastrofuncionarioR :: Handler Html
postCadastrofuncionarioR = do
                ((result, _), _) <- runFormPost formFuncionario
                case result of
                    FormSuccess funcionario -> do
                       unicoEmail <- runDB $ getBy $ UniqueEmail (funcionarioEmail funcionario)
                       case unicoEmail of
                           Just _ -> redirect CadastrofuncionarioR
                           Nothing -> do 
                              funcid <- runDB $ insert funcionario
                              redirect (FuncionarioR funcid)
                    _ -> redirect CadastrofuncionarioR

getFuncionarioR :: FuncionarioId -> Handler Html
getFuncionarioR funcid = do
             funcionario <- runDB $ get404 funcid 
             defaultLayout [whamlet| 
                 <h1> Seja bem-vindo funcionario #{funcionarioNome funcionario}
                 <p> Dados pessoais:
                 <p>
                 <p> Nome: #{funcionarioNome funcionario}
                 <p> Telefone: #{funcionarioTelefone funcionario}
                 <p> CPF:#{funcionarioCpf funcionario}
                 <p> Endereço:#{funcionarioEnd funcionario}
                 <p> E-mail:#{funcionarioEmail funcionario}
             |]
             
postFuncionarioR :: FuncionarioId -> Handler Html
postFuncionarioR funcid = do
     runDB $ delete funcid
     redirect ListarfuncionarioR

getListarfuncionarioR :: Handler Html
getListarfuncionarioR = do
             listaF <- runDB $ selectList [] [Asc FuncionarioNome]
             defaultLayout $ do
                 $(whamletFile "templates/listaf.hamlet") 
                 toWidget $(luciusFile "templates/lista.lucius")

