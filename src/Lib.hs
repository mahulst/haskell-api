{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Text
data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show, Generic)

instance ToJSON User
data SortBy = Age | Name

instance FromHttpApiData SortBy where
  parseQueryParam text =
    case unpack(text)  of
      "age" -> Right Age
      "name" -> Right Name
      _ -> Left (pack "None")

type API = "users" :> QueryParam "sorty" SortBy :> Get '[JSON] [User]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = users
  where users :: Maybe SortBy -> Handler [User]
        users sort =
          case sort of
            Just Age -> return userList
            Just Name -> return (Prelude.reverse userList)
            Nothing -> return []
userList :: [User]
userList = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
