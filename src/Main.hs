{-# LANGUAGE DeriveGeneric, OverloadedStrings  #-}

module Main where

import Data.Text
import Data.Aeson
import Data.Default
import Control.Applicative
-- import Control.Monad
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp, newManager)
import qualified Data.ByteString.Lazy as B

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

main :: IO ()
main = do
    -- json <- (eitherDecode <$> simpleHttp $ (usersUrl ++ username)) :: IO (Either String User)
    json <- (eitherDecode <$> getUser) :: IO (Either String User)
    case json of
        Left error -> putStrLn error
        Right user -> print user


