{-# LANGUAGE OverloadedStrings #-}

{-| This application has a single method to record submitted emails.
    The method accepts form submissions (HTTP POST only). 
    JSON envelope is returned in reply, carrying either errorcode or result.

    SQLite3 is used for data storage. 
    Log messages are to Syslog.
    
    The application requires INI-style /etc/rollout.conf with connString parameter set. connString is a path to SQLite3 database file.
    The database must contain table, named ir:
    CREATE TABLE ir (email TEXT, dated TIMESTAMP);
    
    Sample /etc/rollout.conf:
    connString = Rollout.sqlite3
    insertEmailQuery= "INSERT INTO ir (email,dated) VALUES (?,DATETIME('now'));"
    
    Author: Alexander Abushkevich, alex@abushkevi.ch
    LICENSE: BSD
-}
--------------------------------------------------------------------------------
module Rollout (
    main
) where
--------------------------------------------------------------------------------
import qualified Control.Exception           as EX
import qualified Control.Monad               as CM
import qualified Control.Monad.Reader        as CR
import qualified Control.Monad.IO.Class      as MI
import qualified Data.Aeson                  as AE
import qualified Data.Aeson.Types            as AT
import qualified Data.ByteString.Char8       as BC
import qualified Data.Maybe                  as DM
import qualified Data.Text                   as T
import qualified Database.SQLite3            as SQ
import qualified Database.SQLite.Simple      as SS
import qualified Happstack.Server            as HS
import qualified System.Log.Logger           as SL
import qualified System.Log.Handler.Syslog   as SH
import qualified TConfig                     as CF
import qualified Text.Parsec                 as TP
import qualified Validation                  as VD
--------------------------------------------------------------------------------
-- | Application configuration type
type ConnectionString = String
type EmailInsertQuery = SS.Query
data AppConfig = AppConfig ConnectionString EmailInsertQuery
-- | Default application monad is enhanced to pass configuration
type AppMonad = CR.ReaderT AppConfig (HS.ServerPartT IO)
--------------------------------------------------------------------------------
-- Data types
-- | Envelope for errors, confirmations and computation results
data Envelope a = Envelope (Either ErrorStatus a)
-- | Error code description
data ErrorStatus = ErrorStatus T.Text T.Text
-- | Confirmation that email was recorded successfully
--   IR stands for Interest Registration
data IRConfirmation = IRConfirmation T.Text Bool
--------------------------------------------------------------------------------
-- | Converts envelope and enclosed data into JSON for use in response
instance (AT.ToJSON a) => HS.ToMessage (Envelope a) where
    toContentType _ = BC.pack "application/json"
    toMessage       = AE.encode
--------------------------------------------------------------------------------
-- | Describes how to convert Envelope to JSON
instance (AT.ToJSON a) => AT.ToJSON (Envelope a) where
    toJSON (Envelope (Left  e)) = AT.toJSON e
    toJSON (Envelope (Right a)) = AT.object ["result"  AT..= AT.toJSON a]

-- | Describes how to convert ErrorStatus to JSON
instance AT.ToJSON ErrorStatus where
    toJSON (ErrorStatus code message) = 
        AT.object [ "errorcode"    AT..= AT.String code
                  , "errormessage" AT..= AT.String message]

-- | Describes how to convert IRConfirmation to JSON
instance AT.ToJSON IRConfirmation where
    toJSON (IRConfirmation email confirmed) = 
        AT.object ["confirmed" AT..= AT.Bool   confirmed
                  ,"email"     AT..= AT.String email]
--------------------------------------------------------------------------------
-- | Setup logging, read configuration, start the application
main :: IO ()
main = do
    syslogHandler <- SH.openlog "rollout" [SH.PID] SH.USER SL.DEBUG
    SL.updateGlobalLogger SL.rootLoggerName (SL.addHandler syslogHandler)
    SL.updateGlobalLogger SL.rootLoggerName (SL.setLevel SL.DEBUG)
    
    SL.debugM "rollout.main" "Starting.."
    config <- appConfig
    case config of
        Just c -> 
            HS.simpleHTTP HS.nullConf $ CR.runReaderT app c
        Nothing     -> 
            SL.criticalM "rollout.main" ("Cannot read " ++ appConfigPath)

appPolicy :: HS.BodyPolicy
appPolicy = HS.defaultBodyPolicy "" 0 4096 4096 -- TmpDir Disk RAM Header

appConfigPath :: String
appConfigPath = "/etc/rollout.conf"

-- | Reads application configuration from file
appConfig :: IO (Maybe AppConfig)
appConfig = do
    conf <- CF.readConfig appConfigPath
    let c = CF.getValue "connString"       conf
    let q = CF.getValue "insertEmailQuery" conf
    SL.debugM "rollout.appConfig" ("connString: "       ++ (show c))
    SL.debugM "rollout.appConfig" ("insertEmailQuery: " ++ (show q))
    case (and $ map DM.isJust [c, q]) of
        False -> do
            (SL.criticalM 
                "rollout.appConfig" 
                "connString or insertEmailQuery parameter missing")
            return Nothing
        True  -> return $ Just (AppConfig 
                                    (                    DM.fromJust c) 
                                    (SS.Query $ T.pack $ DM.fromJust q))

-- | High-level application definition
--   Add new routes here
app :: AppMonad HS.Response
app = CM.msum [HS.dir "registerinterest"  registerInterestR]
--------------------------------------------------------------------------------
-- | Converts DB errors to response which is sent to end users
--   Tries to be very concise for "security" purposes.
--   Returns envelope with error message
handleSqliteError :: SQ.SQLError -> IO (Envelope IRConfirmation)
handleSqliteError e = do
    SL.criticalM "rollout.handleSqliteError" (show $ SQ.sqlError e)
    let errorStatus = ErrorStatus 
                        (T.pack "EDATABASE") 
                        (T.pack "Cannot write to database")
    -- XXX: the following may be extended to cover various possible errors
    case (SQ.sqlError e) of
        (SQ.ErrorError) -> return $ Envelope (Left errorStatus)
        _               -> return $ Envelope (Left errorStatus)

-- | Returns envelope with error message
handleInvalidEmail :: T.Text -> TP.ParseError -> IO (Envelope IRConfirmation)
handleInvalidEmail email e = do
    SL.noticeM "rollout.handleInvalidEmail" ("Invalid email: " ++ (show email))
    let errorStatus = ErrorStatus 
                        (T.pack "EVALIDATION") 
                        (T.pack "The email does not seem to be correct")
    return $ Envelope (Left errorStatus)
--------------------------------------------------------------------------------
-- | Write validated email to database
dbWriteEmail :: AppConfig -> T.Text -> IO (Envelope IRConfirmation)
dbWriteEmail (AppConfig connStr query) email = do
    SL.debugM "rollout.saveEmail" ("Saving email: " ++ (show email))
    EX.catches
        (do SS.withConnection connStr (\c -> SS.execute c query (SS.Only email))
            return $ Envelope (Right (IRConfirmation email True))) 
        [EX.Handler handleSqliteError]
    
-- | Save email to database if email is valid or return envelope with error
saveEmail :: AppConfig -> T.Text -> IO (Envelope IRConfirmation)
saveEmail config email = 
    either (handleInvalidEmail email) (dbWriteEmail config) (VD.validEmail email)
--------------------------------------------------------------------------------
-- | Register interest. Takes email from form POST submission, 
--   returns confirmation or error message in JSON format
--   Note, that there is a limit on POST body size - refer to appPolicy.
registerInterestR :: AppMonad HS.Response
registerInterestR = do 
    HS.method HS.POST
    HS.decodeBody appPolicy
    HS.setHeaderM "Server" "ccx"
    config <- CR.ask
    email <- HS.lookText' "email" 
    (MI.liftIO $ saveEmail config email) >>= (HS.ok . HS.toResponse)
--------------------------------------------------------------------------------
