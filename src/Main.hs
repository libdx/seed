{-# LANGUAGE DeriveGeneric, OverloadedStrings  #-}

module Main where

import Control.Exception

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

main :: IO ()
main =
    obtainUser 5 >>= \val ->
        case val of
            Nothing -> putStrLn "Nothing"
            Just x -> putStrLn $ "Just" ++ " " ++ show x

