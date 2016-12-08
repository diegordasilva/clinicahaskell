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
                    
getFuncionarioindexR :: Handler Html
getFuncionarioindexR = do
        defaultLayout $ do
            addStylesheetRemote "https://fonts.googleapis.com/css?family=Lato:300,400,700|Pacifico"
            toWidget [lucius|
        
                body {
                    background-color: #b3cccc;
                }
                h1 { 
                    color: #339933;
		            text-align: center;
		            font-size: 30px;
    		        font-family: 'Pacifico', cursive;
    		    	display: inline-block;
    	    		height: inherit;
        			left: 0;
        			line-height: inherit;
        			margin: 0;
        			padding: 0;
	        		position: absolute;
	        		top: 0;
	        		text-transform: none;
	        		font-weight: normal;
	        		padding: 0;
                    }
        
                ul {
        	        padding:0px;
	                margin:0px;
        	        background-color:#e6f5ff;
        	        list-style:none;
                    }
        
                ul li { display: inline; }
        
                ul li h2 {
            
                    font-size: 25px;
		            font-family: Verdana;
		            display: inline;
                    }
        
                ul li h2 a {
        	        padding: 2px 10px;
        	        display: inline-block;
        	        background-color:#e6f5ff;
        	        color: #333;
        	        text-decoration: none;
        	        border-bottom:3px solid #006633;
                }

                ul li h2 a:hover {
        	        background-color:#D6D6D6;
        	        color: #6D6D6D;
        	        border-bottom:3px solid #EA0000;
                }
        
            |]
            [whamlet|
                <h1> Bem vindo.
                    <ul>
                        <li><h2><a href=@{HelloR}>Home</a>
                        <li><h2><a href=@{CadastropacienteR}>Cadastro de Funcionario</a>
                        <li><h2><a href=@{LoginpacR}>Login</a>
            |]                    


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
                 
                 <p>Cadastrar Médicos:
                 <a href=@{CadastromedicoR}>Cadastro de Médicos</a>
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

