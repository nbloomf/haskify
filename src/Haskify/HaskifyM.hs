{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Haskify.HaskifyM (
  HaskifyM(),

  -- Execution
  runHaskifyM,

  -- HTTP Requests
  urlGet, urlPost, urlPut, urlDelete,

  -- Errors & Logging
  logComment, fieldDNE, tryDecode,

  tryScript -- , retryScript
) where

import Prelude hiding (concat, words)

import System.IO
import Data.Text
import Network.Wreq
import Control.Monad.IO.Class
import Control.Exception
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Control.Lens ((^.), set)
import Network.HTTP.Types.Status (ok200, created201, accepted202)
import Network.HTTP.Client (HttpException(..))

import Haskify.ToURL



{-------------}
{- #HaskifyM -}
{-------------}

newtype HaskifyM t = HM
  { runHM :: State -> IO (Either HMError t, State) }


instance Monad HaskifyM where
  return x = HM (\st -> return (Right x, st))

  x >>= f = HM foo
    where
      foo st1 = do
        (y,st2) <- runHM x st1
        case y of
          Right z  -> runHM (f z) st2
          Left err -> return (Left err, st2)


instance Functor HaskifyM where
  fmap f x = x >>= (return . f)


instance MonadIO HaskifyM where
  liftIO x = HM { runHM = \st -> fmap (\y -> (Right y,st)) x }



{----------}
{- #State -}
{----------}

type State = (Credentials, Log)

-- DO NOT EXPORT
getState :: HaskifyM State
getState = HM { runHM = \st -> return (Right st, st) }

-- DO NOT EXPORT
putState :: State -> HaskifyM ()
putState st = HM { runHM = \_ -> return (Right (), st) }



{- #Credentials -}

data Credentials = Credentials
 { api_key  :: Text
 , password :: Text
 , domain   :: Text
 } deriving Show

emptyCredentials = Credentials
 { api_key  = ""
 , password = ""
 , domain   = ""
 }

readCredentials :: FilePath -> IO Credentials
readCredentials path = do
  [dom,key,pass] <- fmap (words . pack) $ readFile path
  return $ Credentials {domain = dom, api_key = key, password = pass}

instance ToURL Credentials where
  toURL Credentials{..} = concat
    [ "https://", api_key, ":", password, "@", domain ]

-- DO NOT EXPORT
theURL :: Text -> Text -> HaskifyM String
theURL path args = do
  (cred, _) <- getState
  return $ unpack $ concat [toURL cred, path, "?", args]



{- #Log -}

type Log = [LogEntry]

data LogEntry
  = HTTPRequest  HTTPVerb Text ByteString
  | HTTPResponse (Response ByteString)
  | Comment      Text
  deriving Show

data HTTPVerb
  = GET | POST | PUT | DELETE
  deriving Show

logComment :: Text -> HaskifyM ()
logComment msg = do
  liftIO $ putStrLn $ unpack msg
  (cred, log) <- getState
  putState $ (cred, (Comment msg):log)

-- DO NOT EXPORT
logRequest :: HTTPVerb -> Text -> ByteString -> HaskifyM ()
logRequest verb url payload = do
  (cred, log) <- getState
  let req = HTTPRequest verb url payload
  liftIO $ hPutStrLn stderr (show req)
  putState $ (cred, req:log)

-- DO NOT EXPORT
logResponse :: Response ByteString -> HaskifyM ()
logResponse resp = do
  (cred, log) <- getState
  liftIO $ hPutStrLn stderr (show resp)
  putState $ (cred, (HTTPResponse resp):log)



{------------}
{- #HMError -}
{------------}

data HMError
 = JSONDecodingError String ByteString
 | FieldDNE Text
 | HTTPError Text Text (Response ByteString)
 | HTTPError' HttpException
 | GenericError Text Text
 -- | TooManyRetries HMError
 deriving Show

errorHaskifyM :: HMError -> HaskifyM a
errorHaskifyM err = HM { runHM = \st -> return (Left err, st) }

errorWithLocation :: Text -> Text -> HaskifyM a
errorWithLocation loc err = errorHaskifyM $ GenericError loc err

fieldDNE :: Text -> HaskifyM a
fieldDNE msg = errorHaskifyM $ FieldDNE msg



{--------------}
{- #Interface -}
{--------------}

-- must provide path to a credentials file
runHaskifyM :: FilePath -> HaskifyM t -> IO (Either HMError t, Log)
runHaskifyM path script = do
  cred <- liftIO $ readCredentials path
  runHaskifyM' cred script

-- DO NOT EXPORT
runHaskifyM' :: Credentials -> HaskifyM t -> IO (Either HMError t, Log)
runHaskifyM' cred script = do
  (x,(_,log)) <- runHM script (cred,[])
  return (x,log)




{- Catching Errors -}

tryScript :: HaskifyM a -> HaskifyM (Either HMError a)
tryScript script = do
  st <- getState
  (x,_) <- liftIO $ runHM script st
  return x

-- lift HTTP errors
tryHTTP :: HaskifyM a -> HaskifyM (Either (Response ByteString) a)
tryHTTP script = do
  st <- getState
  (x,_) <- liftIO $ runHM script st
  case x of
    Right y -> return (Right y)
    Left (HTTPError _ _ err) -> return (Left err)
    Left err -> errorHaskifyM err 

{-
retryScript :: Int -> HaskifyM a -> HaskifyM (Either HMError a)
retryScript n script = do
  st <- getState
  (x,_) <- liftIO $ runHM script st
  case x of
    Right a  -> return (Right a)
    Left err -> if n <= 0
                  then errorHaskifyM $ TooManyRetries err
                  else retryScript (n-1) script
-}


tryDecode :: (FromJSON t) => ByteString -> HaskifyM t
tryDecode str = case eitherDecode str of
  Right x  -> return x
  Left msg -> errorHaskifyM $ JSONDecodingError msg str


okayStatus :: Status -> Bool
okayStatus x = x `elem` [ok200, created201, accepted202]




wreqOpts :: Options
wreqOpts = set checkStatus (Just $ \_ _ _ -> Nothing) defaults


getURL :: String -> HaskifyM (Response ByteString)
getURL url = do
  logRequest GET (pack url) ""
  let
    attempt = do
      x <- try $ getWith wreqOpts url
      case x of
        Left (FailedConnectionException _ _) -> attempt
        Left err -> error $ show err
        Right x -> return x
  resp <- liftIO attempt
  logResponse resp
  return resp

getURL' :: String -> HaskifyM (Response ByteString)
getURL' url = do
  logRequest GET (pack url) ""
  resp <- liftIO $ getWith wreqOpts url
  logResponse resp
  return resp

postURL :: (ToJSON a) => a -> String -> HaskifyM (Response ByteString)
postURL payload url = do
  logRequest POST (pack url) (encode payload)
  resp <- liftIO $ postWith wreqOpts url (toJSON payload)
  logResponse resp
  return resp

putURL :: (ToJSON a) => a -> String -> HaskifyM (Response ByteString)
putURL payload url = do
  logRequest PUT (pack url) (encode payload)
  resp <- liftIO $ putWith wreqOpts url (toJSON payload)
  logResponse resp
  return resp

deleteURL :: String -> HaskifyM (Response ByteString)
deleteURL url = do
  logRequest DELETE (pack url) ""
  resp <- liftIO $ deleteWith wreqOpts url
  logResponse resp
  return resp

checkResponse :: Text -> Text -> Response ByteString -> HaskifyM ByteString
checkResponse path args response = do
  let status = response ^. responseStatus
  if okayStatus status
    then return (response ^. responseBody)
    else errorHaskifyM $ HTTPError path args response


urlGet :: Text -> Text -> HaskifyM ByteString
urlGet path args =
  theURL path args
    >>= getURL
    >>= checkResponse path args

urlPost :: (ToJSON a) => Text -> Text -> a -> HaskifyM ByteString
urlPost path args payload =
  theURL path args
    >>= postURL payload
    >>= checkResponse path args

urlPut :: (ToJSON a) => Text -> Text -> a -> HaskifyM ByteString
urlPut path args payload =
  theURL path args
    >>= putURL payload
    >>= checkResponse path args

urlDelete :: Text -> Text -> HaskifyM ByteString
urlDelete path args = do
  theURL path args
    >>= deleteURL
    >>= checkResponse path args
