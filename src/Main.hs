{-# LANGUAGE DeriveGeneric, OverloadedStrings  #-}

module Main where

import Control.Exception
import Network.HTTP.Conduit
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as BS

type User = String

database :: FilePath
database = "database"

tryReadFile :: FilePath -> IO (Either IOException String)
tryReadFile path = try $ readFile path

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe path =
    tryReadFile path >>= \smth ->
        case smth of
            Left _ -> return Nothing
            Right content -> return $ Just content

-- fetch from local store
fetchUser :: Int -> IO (Maybe User)
fetchUser _ = readFileMaybe database

-- get from web service
getUser :: Int -> IO (Maybe User)
getUser id = return $ Just $ "pooh" ++ " " ++ show id

-- write to local store and return
writeUser :: Maybe User -> IO (Maybe User)
writeUser maybeUser =
    case maybeUser of
        Just user -> writeFile database user >> return maybeUser
        Nothing -> return maybeUser

obtainUser :: Int -> IO (Maybe User)
obtainUser id = 
    fetchUser id >>= \maybeUser ->
        case maybeUser of
            Just user -> return $ Just user
            Nothing -> getUser id >>= writeUser

username :: String
username = "mojombo"

usersUrl :: String
usersUrl = "https://api.github.com/users/"

userAgent :: BS.ByteString
userAgent = "haskell-bot"

setUserAgent :: Request -> BS.ByteString -> Request
setUserAgent request agent =
    let headers = requestHeaders request
    in request {
            requestHeaders = ("User-agent", agent) : headers
       }

main :: IO ()
main = withManager $ \manager -> do
    initRequest <- parseUrl $ usersUrl ++ username
    request <- return $ setUserAgent initRequest userAgent
    response <- httpLbs request manager
    liftIO $ print $ responseBody response

