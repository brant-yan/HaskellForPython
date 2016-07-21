{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric  #-} --json
module ScottyExample where

import GHC.Generics --json
import Data.Aeson hiding(json) --json


import Web.Scotty
import Network.Wai.Middleware.RequestLogger 

import Control.Monad
import Control.Monad.Trans
import Data.Monoid
import System.Random (newStdGen, randomRs)

import Network.HTTP.Types (status302)

import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.String (fromString)

import Control.Applicative



readInt :: String -> Int
readInt str = read str

toOpt :: String -> (Int->Int->Int)
toOpt "add" = (+) 
toOpt "sub" = (-)
toOpt "mlt" = (*)
toOpt  o    = (+)   

toResponse :: Either ErrorMessage a -> Response
toResponse (Left msg) = Response msg 400
toResponse (Right _)  = Response "ok" 200

data Response = Response{message :: ErrorMessage , code:: Int} deriving (Show,Generic) --add Generic for json
instance ToJSON Response--json


type ErrorMessage = String

data UserInfo = UserInfo UserName Password deriving (Show)

class Validatable a where
    validate :: a -> Either ErrorMessage a
    validate d = Left "Value is Error Format" 

data UserName = UserName String deriving (Show)
data Password = Password String deriving (Show)

instance Validatable UserName where
    validate a@(UserName "brant")   = Right a
    validate _                      = Left "User not registe"

instance Validatable Password where
    validate (Password "")      = Left "Password is null"
    validate pwd                = Right pwd



validateUserNameAndPassword :: String -> String -> Either ErrorMessage UserInfo
validateUserNameAndPassword username password = liftA2 UserInfo (validate $ UserName username) (validate $ Password password)

main1 :: IO ()
main1 = scotty 3000 $ do
            middleware logStdoutDev
            get "/" $ text "foobar"
            get "/favicon.ico" $ file "favicon.ico"
            get "/index.html" $ file "index.html"
            get "/computer/:first/:second" $ do
                first <- param "first"
                second <- param "second"
                json $ readInt first + readInt second
            get "/computer" $ do
                first <- param "first"
                second <- param "second"
                opt <- param "opt"
                --json $ uncurry (toOpt opt) (readInt first ,readInt second)
                json $ (toOpt opt) (readInt first) (readInt second)
            get "/login" $ do
                name <- param "username"
                pwd <- param "password"
                json $ toResponse (validateUserNameAndPassword name pwd)


