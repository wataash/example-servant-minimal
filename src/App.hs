{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Data.Aeson (ToJSON, FromJSON)
import           GHC.Generics (Generic)
import           Network.Wai
import           Network.Wai.Handler.Warp (setPort, setBeforeMainLoop, defaultSettings, runSettings)
import           Servant -- ((:>), Get, JSON, (:<|>), Capture, Proxy, Server, Handler, serve, throwError, err404)
import           System.IO (hPutStrLn, stderr)

-- * api

type ItemApi =
  "item" :> Get '[JSON] [Item] :<|>
  "item" :> Capture "itemId" Integer :> Get '[JSON] Item

-- http://localhost:3000/item
itemApi :: Proxy ItemApi
itemApi = Proxy

-- * app

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve itemApi server

-- [{"itemId":0,"itemText":"example item"}]
-- itemApi ->
server :: Server ItemApi
server =
  getItems :<|>
  getItemById

-- http://localhost:3000/item   [{"itemId":0,"itemText":"example item"}]
-- itemApi -> server ->
getItems :: Handler [Item]
getItems = return [exampleItem] -- 

-- http://localhost:3000/item/0  {"itemId":0,"itemText":"example item"}
-- http://localhost:3000/item/1  404
-- itemApi ->  server ->
getItemById :: Integer -> Handler Item
getItemById = \ case
  0 -> return exampleItem
  _ -> throwError err404

-- itemApi -> server -> getItems -> 
exampleItem :: Item
exampleItem = Item 0 "example item"

-- * item

data Item
  = Item {
    itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item

data a + b = Foo a b

type X = Int + Bool
