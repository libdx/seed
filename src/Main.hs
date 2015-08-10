{-# LANGUAGE DeriveGeneric, OverloadedStrings  #-}

module Main where

import Control.Exception
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
-- import Control.Monad.IO.Class
-- import Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

type User = String

database :: FilePath
database = "database"

defaultUsername :: String
defaultUsername = "mojombo"

usersUrl :: String
usersUrl = "https://api.github.com/users_/"

userAgent :: BS.ByteString
userAgent = "haskell-bot"

bytes = encodeUtf8 . Text.pack
string = Text.unpack . decodeUtf8
strict = BL.toStrict

setUserAgent :: Request -> BS.ByteString -> Request
setUserAgent request agent =
    let headers = requestHeaders request
    in request {
            requestHeaders = ("User-agent", agent) : headers
       }

setDontCheckStatus :: Request -> Request
setDontCheckStatus request =
    request { checkStatus = \_ _ _ -> Nothing }

tryReadFile :: FilePath -> IO (Either IOException String)
tryReadFile path = try $ readFile path

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe path =
    tryReadFile path >>= \smth ->
        case smth of
            Left _ -> return Nothing
            Right content -> return $ Just content

stringResponseBody :: Response BL.ByteString -> String
stringResponseBody = string . strict . responseBody

-- fetch from local store
fetchUser :: String -> IO (Maybe User)
fetchUser _ = readFileMaybe database

configureRequest :: Request -> Request
configureRequest request = setDontCheckStatus $ setUserAgent request userAgent

-- get from web service
getUser :: String -> IO (Maybe User)
getUser username = withManager $ \manager -> do
    request' <- parseUrl $ usersUrl ++ username
    let request = configureRequest request'

    response <- httpLbs request manager
    statusCode <- return $ statusCode $ responseStatus response
    if 200 <= statusCode && statusCode < 300
        then return $ Just $ stringResponseBody response
        else return Nothing

-- write to local store and return
writeUser :: Maybe User -> IO (Maybe User)
writeUser maybeUser =
    case maybeUser of
        Just user -> writeFile database user >> return maybeUser
        Nothing -> return maybeUser

obtainUser :: String -> IO (Maybe User)
obtainUser username = 
    fetchUser username >>= \maybeUser ->
        case maybeUser of
            Just user -> return $ Just user
            Nothing -> getUser username >>= writeUser

main :: IO ()
main = obtainUser defaultUsername >>= \user ->
    case user of
        Nothing -> putStrLn "Nothing"
        Just user -> putStrLn user
   

