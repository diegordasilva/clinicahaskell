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



getCadastropacienteR :: Handler Html
getCadastropacienteR = do
             (widget, enctype) <- generateFormPost formPaciente
             defaultLayout $ do 
                -- addStylesheet $ StaticR style_css
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
                    
getPacienteindexR :: Handler Html
getPacienteindexR = do
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
                        <li><h2><a href=@{CadastropacienteR}>Primeiro acesso</a>
                        <li><h2><a href=@{LoginpacR}>Login</a>
            |]

getPacienteR :: PacienteId -> Handler Html
getPacienteR pacid = do
             paciente <- runDB $ get404 pacid 
             defaultLayout $ do 
             addStylesheetRemote "https://fonts.googleapis.com/css?family=Lato:300,400,700|Pacifico"      
             [whamlet| 
                <body style="background-color: #b3cccc;"> 
                     <span style="font-family:Verdana;">
                        <h1 style="font-size: 30px;">Seja bem-vindo #{pacienteNome paciente}
                     <br>
                     <h2 style="font-family:Verdana"> Dados pessoais:
                     <br>
                     <p style="font-family:Verdana"> Nome: #{pacienteNome paciente}
                     <p style="font-family:Verdana"> Telefone: #{pacienteTelefone paciente}
                     <p style="font-family:Verdana"> CPF:##{pacienteCpf paciente}
                     <p style="font-family:Verdana"> Endereço:#{pacienteEnd paciente}
                     <p style="font-family:Verdana"> E-mail:#{pacienteEmail paciente}
                     
                     <p style="font-family:Verdana"> Fazer agendamento de consulta: <a href=@{AgendamentoR}>Agendamento</a>
                     <br>
                     <span style="font-family:Verdana"><a href=@{HelloR}><button>Home</button></a></span>
             |]
             
postPacienteR :: PacienteId -> Handler Html
postPacienteR pacid = do
     runDB $ delete pacid
     redirect ListarpacienteR

getListarpacienteR :: Handler Html
getListarpacienteR = do
             listaP <- runDB $ selectList [] [Asc PacienteNome]
             defaultLayout $ do
                 $(whamletFile "templates/lista.hamlet") 
                 toWidget $(luciusFile "templates/lista.lucius")

