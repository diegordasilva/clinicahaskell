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
        
        teste {
            
            background-color:#EA0000;
            
        }
        
    |]
    [whamlet|
        <div #teste>
        <h1>Clinica Haskell
            <ul>
                <li><h2><a href=@{HelloR}>Home</a>
                <li><h2><a href=@{CadastropacienteR}>Cadastro de Pacientes</a>
                <li><h2> <a href=@{CadastrofuncionarioR}>Cadastro de Funcionarios</a>
                <li><h2> <a href=@{LoginR}>Login</a>
            $maybe sess <- sessao
                <form method=post action=@{LogoutR}>
                    <input type="submit" value="Logout">
    |]
    
    {-[whamlet|
        <h1>Clinica Haskell
           <body #homepage>
                <div #site>
                    <ul>
                        <li><a href=@{HelloR}>Homepage
                        <li><a href=@{CadastrofuncionarioR}>Cadastro de Funcionarios
                        <li><a href=@{CadastropacienteR}>Cadastro de Pacientes 
                    <h3>Area de Pacientes
                    <section #4u>
                        <span #pennant><span #fa fa-lock>
                            <h3>Area de Funcionarios
                            <p>Texto funcionarios.
                    <section #4u>
                        <span #pennant><span #fa fa-globe>
                            <h3>Agendamento de consultas
                            <p>Nesta area é possivel agendar consultas.
                            <a href=@{AgendamentoR} >Saiba mais
                    <div #site>
                    <p>Trabalho de Haskell desenvolvido por Bruno Felippe, Diego Rogrigues e Thor Cortes. 2016.

|]-}
