{-# LANGUAGE DeriveGeneric, OverloadedStrings  #-}

module Main where

import Data.Text
import Data.Aeson
-- import Data.Default
import Data.Conduit
import qualified Data.Conduit.Binary as CB
-- import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import GHC.Generics
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS

username :: String
username = "mojombo"

usersUrl :: String
usersUrl = "https://api.github.com/users/"

getUser :: IO B.ByteString
getUser = simpleHttp $ usersUrl ++ username

data User = User { id            :: Int
                 , login         :: !Text
                 , site_admin    :: Bool
                 } deriving (Show, Generic)

instance FromJSON User
instance ToJSON User

adminText :: User -> String
adminText user = if site_admin user then "Admin" else "Normal"

parseSink :: Sink BS.ByteString (ResourceT IO) ()
parseSink = do
    md <- await
    case md of
        Nothing -> return ()
        Just smth -> do
            liftIO $ BS.putStrLn smth
            parseSink

main :: IO ()
main = do
    manager <- newManager conduitManagerSettings
    request <- parseUrl $ usersUrl ++ username
    let headers = requestHeaders request
        request' = request {
            requestHeaders =
                ("User-agent", "haskell-bot") : headers
        }
    runResourceT $ do
        response <- http request' manager
        responseBody response $$+- CB.lines =$ parseSink
   

    -- json <- (eitherDecode <$> getUser) :: IO (Either String User)
    -- case json of
    --     Left error -> putStrLn error
    --     Right user -> print user

