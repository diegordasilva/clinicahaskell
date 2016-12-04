{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell, ViewPatterns #-}
 
module Application where

import Yesod
import Foundation
import Handlers.Agendamento
import Handlers.Login
import Handlers.Medico
import Handlers.Paciente

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" resourcesSitio

getHelloR :: Handler Html
getHelloR = defaultLayout $ do
     sess <- lookupSession "_ID"
     [whamlet|
         <h1> Clinica Haskell
         <meta http-equiv="content-type" content="text/html; charset=utf-8" />
         <meta name="description" content="" />
         <meta name="keywords" content="" />
         <link href='http://fonts.googleapis.com/css?family=Roboto:400,100,300,700,500,900' rel='stylesheet' type='text/css'>
         <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js">
          <script src="js/skel.min.js">
          <script src="js/skel-panels.min.js"> 
          <script src="js/init.js">
          <noscript>
          <link rel="stylesheet" href="css/skel-noscript.css" />
          <link rel="stylesheet" href="css/style.css" />
           <link rel="stylesheet" href="css/style-desktop.css" />
           <body class="homepage">

               <div id="header">
                 <div id="nav-wrapper"> 

                  <nav id="nav">
                      <ul>
                         <li class="active"><a href=@{HelloR}>Homepage
                           <li><a href="contato.html">Contato
                            <li><a href="sobre.html">Sobre
                             <li><a href="login.html">Login
                      <div class="container"> 
                      <!-- Logo -->
                       <div id="logo">
                        <h1><a href="#">Clínica Haskell

<div id="featured">
<div class="container">
<div class="row">
<section class="4u">
<span class="pennant">
<span class="fa fa-briefcase">
<h3>Linguiça 1
<p>Pacientes
<a href=@{CadastropacienteR} class="button button-style1">Cadastro de Pacientes
<section class="4u">
<span class="pennant"><span class="fa fa-lock">
<h3>Linguiça 2
<p>Donec ornare neque ac sem. Mauris aliquet. Aliquam sem leo, vulputate sed, convallis at, ultricies quis, justo. Donec magna.
<a href="linguica2.html" class="button button-style1">Saiba mais

<section class="4u">
<span class="pennant"><span class="fa fa-globe">
<h3>Linguiça 3
<p>Curabitur sit amet nulla. Nam in massa. Sed vel tellus. Curabitur sem urna, consequat vel, suscipit in, mattis placerat, nulla. Sed ac leo.
<a href="linguica3.html" class="button button-style1">Saiba mais

<!-- Copyright -->
<div id="copyright">
<div class="container">
Trabalho de Haskell desenvolvido por Bruno Felippe, Diego Rogrigues e Thor Cortes. 2016.

|]
