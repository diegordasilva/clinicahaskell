{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell, ViewPatterns #-}
 
module Application where

import Yesod
import Foundation
import Handlers.Agendamento
import Handlers.Login
import Handlers.Medico
import Handlers.Paciente
import Handlers.Funcionario

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" resourcesSitio

getHelloR :: Handler Html
getHelloR = defaultLayout $ do
    sessao <- lookupSession "_ID"
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
        
        p {
            
            font-family:verdana;
            text-align: center;
            color: #1f2e2e;
            font-size: 15px;
            
        }
        
    |]
    [whamlet|
        <h1>Clinica Haskell
            <ul>
                <li><h2><a href=@{HelloR}>Home</a>
                <li><h2><a href=@{PacienteindexR}>Area de Pacientes</a>
                <li><h2> <a href=@{FuncionarioindexR}>Area de Funcionarios</a>
                
                
            <p>  Sua saúde não pode parar.
            <p>Nossa equipe, com dezenas de médicos credenciados, atua em todas as especialidades nas cidades da Baixada Santista.
            $maybe sess <- sessao
                <form method=post action=@{LogoutR}>
                    <input type="submit" value="Logout">
    |]
