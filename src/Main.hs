{-# LANGUAGE DeriveGeneric, OverloadedStrings  #-}

module Main where

import System.IO
import Data.Aeson
import Data.Text
import qualified Data.List as List
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

bytes = encodeUtf8 . Text.pack
string = Text.unpack . decodeUtf8
strict = BL.toStrict
lazy = BL.fromStrict

{- network utils -}

type JSONString = BS.ByteString

data HttpMethod = GET | POST | PUT | PATCH | DELETE deriving (Enum, Show)

defaultUsername :: String
defaultUsername = "mojombo"

endpoint :: String
endpoint = "https://api.github.com"

usersPath :: String
usersPath = "users"

reposPath :: String
reposPath = "repos"

userAgent :: BS.ByteString
userAgent = "haskell-bot"

configureRequest :: HttpMethod -> Request -> Request
configureRequest method request =
    let headers = requestHeaders request
    in request
        { requestHeaders = ("User-agent", userAgent) : headers
          , checkStatus = \_ _ _ -> Nothing
          , method = bytes $ show method
        }

stringResponseBody :: Response BL.ByteString -> String
stringResponseBody = string . strict . responseBody

strictResponseBody :: Response BL.ByteString -> BS.ByteString
strictResponseBody = strict . responseBody

-- get from web service
getUser :: String -> IO (Maybe User)
getUser username = decodeJSON =<< (apiCall GET [usersPath, username])

getUsers :: IO (Maybe JSONString)
getUsers = apiCall GET [usersPath]

getRepo :: String -> IO (Maybe JSONString)
getRepo username = apiCall GET [usersPath, username, reposPath]

apiCall :: HttpMethod -> [String] -> IO (Maybe JSONString)
apiCall method paths {-body-} =
    runRequest =<< (makeRequest method $ endpoint ++ (makeUrl paths))

makeUrl :: [String] -> String
makeUrl paths = '/' : List.intercalate "/" paths

makeRequest :: MonadThrow m => HttpMethod -> String -> m Request
makeRequest method url = do
    request <- parseUrl url
    return $ configureRequest method request

runRequest :: Request -> IO (Maybe JSONString)
runRequest request = withManager $ \manager -> do
    response <- httpLbs request manager
    statusCode <- return $ statusCode $ responseStatus response
    if 200 <= statusCode && statusCode < 300
        then return $ Just $ strictResponseBody response
        else return Nothing
 
{- file utils -}

database :: FilePath
database = "database"

tryReadFile :: FilePath -> IO (Either EX.IOException BS.ByteString)
tryReadFile path = EX.try $ BS.readFile path

maybeReadFile :: FilePath -> IO (Maybe BS.ByteString)
maybeReadFile path =
    tryReadFile path >>= \smth ->
        case smth of
            Left _ -> return Nothing
            Right content -> return $ Just content

-- decodeWithMaybe :: FromJSON a => Maybe BS.ByteString -> Maybe a
-- decodeWithMaybe maybeJson = 
--     case maybeJson of
--         Just json -> decodeStrict json
--         Nothing -> Nothing

decodeJSON :: FromJSON a => Maybe BS.ByteString -> IO (Maybe a)
decodeJSON maybeJson =
    case maybeJson of
        Just json -> return $ decodeStrict json
        Nothing -> return $ Nothing

-- fetch from local store
fetchUser :: String -> IO (Maybe User)
fetchUser _ = decodeJSON =<< maybeReadFile database

-- write to local store and return
writeRecord :: ToJSON a => Maybe a -> IO (Maybe a)
writeRecord maybeRecord =
    case maybeRecord of
        Just record -> (BS.writeFile database $ (strict. encode) record) >> return maybeRecord
        Nothing -> return Nothing

obtainUser :: String -> IO (Maybe User)
obtainUser username = 
    fetchUser username >>= \maybeUser ->
        case maybeUser of
            Just user -> return $ Just user
            Nothing -> getUser username >>= writeRecord

data User = User { id           :: Int
                 , login        :: !Text
                 , avatar_url   :: !Text
                 , site_admin   :: Bool
                 } deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

data Repo = Repo { repoId       :: Int
                 , repoName     :: !Text
                 , repoPrivate  :: Bool
                 } deriving (Show, Generic)

instance FromJSON Repo
instance ToJSON Repo

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
