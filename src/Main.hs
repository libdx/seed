{-# LANGUAGE DeriveGeneric, OverloadedStrings  #-}

module Main where

import System.IO
import Data.Aeson
import Data.Text
-- import Control.Applicative
import GHC.Generics
import qualified Control.Exception as EX
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
-- import Control.Monad.IO.Class
-- import Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Monad.Catch

type JSONString = BS.ByteString

database :: FilePath
database = "database"

defaultUsername :: String
defaultUsername = "mojombo"

usersUrl :: String
usersUrl = "https://api.github.com/users/"

reposUrlPart :: String
reposUrlPart = "repos"

userAgent :: BS.ByteString
userAgent = "haskell-bot"

bytes = encodeUtf8 . Text.pack
string = Text.unpack . decodeUtf8
strict = BL.toStrict
lazy = BL.fromStrict

setUserAgent :: Request -> BS.ByteString -> Request
setUserAgent request agent =
    let headers = requestHeaders request
    in request {
            requestHeaders = ("User-agent", agent) : headers
       }

setDontCheckStatus :: Request -> Request
setDontCheckStatus request =
    request { checkStatus = \_ _ _ -> Nothing }

tryReadFile :: FilePath -> IO (Either EX.IOException BS.ByteString)
tryReadFile path = EX.try $ BS.readFile path

readFileMaybe :: FilePath -> IO (Maybe BS.ByteString)
readFileMaybe path =
    tryReadFile path >>= \smth ->
        case smth of
            Left _ -> return Nothing
            Right content -> return $ Just content

stringResponseBody :: Response BL.ByteString -> String
stringResponseBody = string . strict . responseBody

strictResponseBody :: Response BL.ByteString -> BS.ByteString
strictResponseBody = strict . responseBody

decodeMaybe :: FromJSON a => Maybe BS.ByteString -> Maybe a
decodeMaybe maybeJson = 
    case maybeJson of
        Just json -> decodeStrict json
        Nothing -> Nothing

-- fetch from local store
fetchUser :: String -> IO (Maybe User)
fetchUser _ = readFileMaybe database >>= \json -> return $ decodeMaybe json

-- get from web service
getUser :: String -> IO (Maybe User)
getUser username = (runGetRequest $ usersUrl ++ username) >>= \json -> return $ decodeMaybe json

getUsers :: IO (Maybe JSONString)
getUsers = runGetRequest $ usersUrl

getRepo :: String -> IO (Maybe JSONString)
getRepo username = runGetRequest $ usersUrl ++ "/" ++ username ++ "/" ++ reposUrlPart

runGetRequest :: String -> IO (Maybe JSONString)
runGetRequest url = do
    request <- makeRequest url
    runRequest request

configureRequest :: Request -> Request
configureRequest request = setDontCheckStatus $ setUserAgent request userAgent

makeRequest :: MonadThrow m => String -> m Request
makeRequest url = do
    request' <- parseUrl url
    return $ configureRequest request'

runRequest :: Request -> IO (Maybe JSONString)
runRequest request = withManager $ \manager -> do
    response <- httpLbs request manager
    statusCode <- return $ statusCode $ responseStatus response
    if 200 <= statusCode && statusCode < 300
        then return $ Just $ strictResponseBody response
        else return Nothing
 
-- write to local store and return
writeUser :: Maybe User -> IO (Maybe User)
writeUser maybeUser =
    case maybeUser of
        Just user -> (BS.writeFile database $ encodeStrict user) >> return maybeUser
        Nothing -> return maybeUser
    where
        encodeStrict = strict . encode

obtainUser :: String -> IO (Maybe User)
obtainUser username = 
    fetchUser username >>= \maybeUser ->
        case maybeUser of
            Just user -> return $ Just user
            Nothing -> getUser username >>= writeUser

data User = User { id           :: Int
                 , login        :: !Text
                 , avatar_url   :: !Text
                 , site_admin   :: Bool
                 } deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

main :: IO ()
main = obtainUser defaultUsername >>= \user ->
    case user of
        Nothing -> print "Nothing"
        Just user -> print user

-- Add promt and repl functions for future use
prompt :: String -> IO String
prompt string = do
    putStr string
    hFlush stdout
    getLine

repl :: IO ()
repl = do
    input <- prompt "> "
    if input == "exit"
        then return ()
        else do
            putStrLn "type exit if you want escape"
            repl
