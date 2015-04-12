{-# LANGUAGE DeriveGeneric, OverloadedStrings  #-}

module Main where

import Control.Exception
import Network.HTTP.Conduit
-- import Control.Monad.IO.Class (liftIO)
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
usersUrl = "https://api.github.com/users/"

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

tryReadFile :: FilePath -> IO (Either IOException String)
tryReadFile path = try $ readFile path

readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe path =
    tryReadFile path >>= \smth ->
        case smth of
            Left _ -> return Nothing
            Right content -> return $ Just content

-- fetch from local store
fetchUser :: String -> IO (Maybe User)
fetchUser _ = readFileMaybe database

-- get from web service
getUser :: String -> IO (Maybe User)
getUser username = withManager $ \manager -> do
    -- TODO: check for errors in response
    initRequest <- parseUrl $ usersUrl ++ username
    request <- return $ setUserAgent initRequest userAgent
    response <- httpLbs request manager
    return $ Just $ string $ strict $ responseBody response

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
   

