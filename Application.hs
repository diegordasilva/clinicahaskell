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
    toWidget [lucius|
        
        h1 { 
            color: #339933;
		    text-align: center;
		    font-family: "Verdana";
		    font-size: 30px;
        }
        
        ul {
	        padding:0px;
	        margin:0px;
	        background-color:#EDEDED;
	        list-style:none;
        }
        
        ul li { display: inline; }
        
        ul li a {
	        padding: 2px 10px;
	        display: inline-block;
	        background-color:#EDEDED;
	        color: #333;
	        text-decoration: none;
	        border-bottom:3px solid #EDEDED;
        }

        ul li a:hover {
	        background-color:#D6D6D6;
	        color: #6D6D6D;
	        border-bottom:3px solid #EA0000;
        }
    |]
    [whamlet|
        <h1> Clinica Haskell! 
            <ul>
                <li> <a href=@{HelloR}>Home</a>
                <li> <a href=@{CadastropacienteR}>Cadastro de Pacientes</a>
                <li> <a href=@{CadastrofuncionarioR}> Cadastro de Funcionarios</a>
                <li> <a href=@{LoginR}>Login</a>
            $maybe sess <- sessao
                <form method=post action=@{LogoutR}>
                    <input type="submit" value="Logout">
    |]
    {-[whamlet|
        <h1> Clinica Haskell
           <body class="homepage">
                <div id="site">
                    <ul>
                        <li><a href=@{HelloR}>Homepage
                        <li><a href=@{CadastrofuncionarioR} class="button button-style1">Cadastro de Funcionarios
                        <li><a href=@{CadastropacienteR} class="button button-style1">Cadastro de Pacientes 
                    <h3>Area de Pacientes
                    <section class="4u">
                        <span class="pennant"><span class="fa fa-lock">
                            <h3>Area de Funcionarios
                            <p>Texto funcionarios.
                    <section class="4u">
                        <span class="pennant"><span class="fa fa-globe">
                            <h3>Agendamento de consultas
                            <p>Nesta area é possivel agendar consultas.
                            <a href=@{AgendamentoR} class="button button-style1">Saiba mais
                    <div id="site">
                    <p>Trabalho de Haskell desenvolvido por Bruno Felippe, Diego Rogrigues e Thor Cortes. 2016.

|]-}
