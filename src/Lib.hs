{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp
  , app
  ) where

import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show, Generic)

data Recipe = Recipe
  { recipeText  :: Text
  , ingredients :: [Ingredient]
  } deriving (Show, Generic)

data Ingredient = Ingredient
  { ingredientName :: Text
  , howMuch        :: Text
  , category       :: Category
  } deriving (Show, Generic)

data Category
  = Vegetable
  | Diary
  | Meat
  | Sweet
  | Bread
  | General
  deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

instance ToJSON Recipe

instance ToJSON Ingredient

instance ToJSON Category


data SortBy
  = Age
  | Name

instance FromHttpApiData SortBy where
  parseQueryParam text =
    case unpack text of
      "age"  -> Right Age
      "name" -> Right Name
      _      -> Left (pack "None")

type API
   = "users" :> QueryParam "sorty" SortBy :> Get '[ JSON] [User] :<|> "recipes" :> Get '[ JSON] [Recipe] :<|> "users" :> "add" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe User)

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = users :<|> recipes :<|> userAdd
  where
    users :: Maybe SortBy -> Handler [User]
    users sort =
      case sort of
        Just Age  -> return userList
        Just Name -> return (Prelude.reverse userList)
        Nothing   -> return []
    recipes :: Handler [Recipe]
    recipes = return recipeList
    userAdd :: User -> Handler (Maybe User)
    userAdd newUser = return (Just newUser)


userList :: [User]
userList = [User 1 "Isaac" "Newton", User 2 "Albert" "Einstein"]

recipeList :: [Recipe]
recipeList =
  [ Recipe
      (pack "This is a recipe for Pancakes")
      [ Ingredient (pack "Flour") (pack "250 gram") General
      , Ingredient (pack "Eggs") (pack "4") Diary
      ]
  ]
