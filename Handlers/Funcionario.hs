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
             areq textField     "Nome" Nothing <*>
             areq intField      "Telefone" Nothing <*>
             areq intField      "CPF" Nothing <*>
             areq textField     "Endereço" Nothing <*>
             areq emailField    "E-mail" Nothing <*>
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
                        <li><h2><a href=@{CadastrofuncionarioR}>Cadastro de Funcionario</a>
                        <li><h2><a href=@{CadastromedicoR}>Cadastro de Medicos</a>
                        <li><h2><a href=@{LoginfuncR}>Login</a>
                        <li><h2><a href=@{ListarmedicoR}>Lista de médicos</a>
                        <li><h2><a href=@{ListarpacienteR}>LIsta de Pacientes</a>
            |]                    


getFuncionarioR :: FuncionarioId -> Handler Html
getFuncionarioR funcid = do
             funcionario <- runDB $ get404 funcid 
             defaultLayout $ do 
             [whamlet|
                <body style="background-color: #b3cccc;"> 
                     <span style="font-family:Verdana;">
                        <h1 style="font-size: 30px;"> Seja bem-vindo funcionario #{funcionarioNome funcionario}
                 <br>
                 <p style="font-family:Verdana"> Dados pessoais:
                 <br>
                 <p style="font-family:Verdana"> Nome: #{funcionarioNome funcionario}
                 <p style="font-family:Verdana"> Telefone: #{funcionarioTelefone funcionario}
                 <p style="font-family:Verdana"> CPF:#{funcionarioCpf funcionario}
                 <p style="font-family:Verdana"> Endereço:#{funcionarioEnd funcionario}
                 <p style="font-family:Verdana"> E-mail:#{funcionarioEmail funcionario}
                 
                 <p style="font-family:Verdana">Cadastrar Médicos:<a href=@{CadastromedicoR}>Cadastro de Médicos</a>
                 <br>
                 <span style="font-family:Verdana"><a href=@{HelloR}><button>Home</button></a></span>
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

