{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable, EmptyDataDecls,
             GeneralizedNewtypeDeriving, ViewPatterns, FlexibleInstances #-}

module Foundation where
import Yesod
import Yesod.Static
import Data.Text
--import Data.Time.Calendar
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio {getStatic :: Static, connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

Funcionario
    nome Text
    telefone Int
    cpf Int
    end Text
    email Text
    senha Text
    deriving Show

Medico
   nome Text
   especialidade Text
   crm Int
   deriving Show

Paciente
   nome Text
   telefone Int
   cpf Int
   end Text
   email Text
   senha Text
   UniqueEmail email
   deriving Show
   
Agendamento
    pacid PacienteId
    medid MedicoId
   -- dia   Day
    deriving Show
   
|]

staticFiles "static"

mkYesodData "Sitio" $(parseRoutesFile "routes")

mkMessage "Sitio" "messages" "pt-br"

instance YesodPersist Sitio where
    type YesodPersistBackend Sitio = SqlBackend
    runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Sitio where
    authRoute _ = Just LoginfuncR
    isAuthorized LoginpacR _ = return Authorized
    isAuthorized LoginfuncR _ = return Authorized
    isAuthorized HelloR _ = return Authorized
    isAuthorized CadastropacienteR _ = return Authorized
    isAuthorized CadastrofuncionarioR _ = return Authorized
    isAuthorized PacienteindexR _ = return Authorized
    isAuthorized FuncionarioindexR _ = return Authorized
    isAuthorized _ _ = estaAutenticado

estaAutenticado :: Handler AuthResult
estaAutenticado = do
   msu <- lookupSession "_ID"
   case msu of
       Just _ -> return Authorized
       Nothing -> return AuthenticationRequired

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
   renderMessage _ _ = defaultFormMessage

-- FUNCAO PARA GERAR FORMULARIOS DE UMA MANEIRA GENERICA
widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = $(whamletFile "templates/form.hamlet")
