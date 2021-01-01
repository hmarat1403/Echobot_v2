{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, OverloadedStrings, GADTs #-}
module VkAPI where

import qualified Data.Text as T
import Prelude hiding ( id )
import GHC.Generics (Generic) 
import Data.Aeson ((.:), withObject, eitherDecode,  genericParseJSON
                  , genericToJSON
                  , FromJSON (parseJSON)
                  , ToJSON (toJSON)
                  , defaultOptions
                  , Options (..) )
import Network.HTTP.Simple
    ( Request, parseRequest_, Response, getResponseBody )
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Aeson.Types (Value(Object))




{- getDecodeUpdate :: Response LBC.ByteString -> VkResponse    -- parse response from JSON
getDecodeUpdate reseivingBC = let jsonBody = getResponseBody reseivingBC
                                  vkResponse = eitherDecode jsonBody
                              in case vkResponse of 
                                    Left noDec -> error $ "can't decode last reseiving update: " <> noDec
                                    Right res  -> res

requestWith :: Request
requestWith = parseRequest_ $ vkRequest key1 ts1 

vkRequest :: String -> String -> String
vkRequest key ts = "https://lp.vk.com/wh201350386?act=a_check&wait=25&mode=2&key=" <> key <> "&ts=" <> ts

key1 :: String
key1 = "f72bec46cbeefe48b841e4ca2b870fd99ed97591"
ts1 :: String
ts1 = "38" -}

data VkUpdates = VkUpdates
                 { ts :: T.Text 
                 , updates :: [Update]
                 } deriving (Show , Generic)
instance FromJSON VkUpdates
--instance ToJSON VkUpdates

data Update = Update
              { _type :: T.Text 
              , _object :: Object
              , _group_id :: Int 
              , _event_id :: T.Text 
              } deriving Show
{- instance FromJSON Update where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}
instance ToJSON Update where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1} -}        

data Object = ObjMessageNew MessageNew
            | ObjMessageReply MessageReply 
            | ObjMessageEvent MessageEvent 
            | ObjPhoto Photo
            | ObjVideo Video
            | ObjAudio Audio
            | ObjWallPost WallPost
                deriving Show 

instance FromJSON Update where
    parseJSON = withObject "update" $ \v -> do
        _type <- v .: "type" 
        _object <- case _type :: T.Text of 
            "message_new" -> ObjMessageNew <$> v .: "object"
            "message_reply" -> ObjMessageReply <$> v .: "object"
            "message_event" -> ObjMessageEvent <$> v .: "object"
            "photo_new" -> ObjPhoto <$> v .: "object"
            "video_new" -> ObjVideo <$> v .: "object"
            "audio_new" -> ObjAudio <$> v .: "object"
            "wall_post_new" -> ObjWallPost <$> v .: "object"
        _group_id <- v .: "group_id"
        _event_id <- v .: "event_id"
        return Update{..}
{- data MessageNew = MessageNewOldVer
                | MessageNewNewVer { message :: PrivateMessage
                                   , client_info :: Maybe ClientInfo
                                   } deriving (Show , Generic)
instance FromJSON MessageNew 
instance ToJSON MessageNew -}

type MessageNew = PrivateMessage
type MessageReply = PrivateMessage

data PrivateMessage = PrivateMessage
                      { id :: Int 
                      , date :: Int 
                      , out :: Maybe Int
                      , user_id :: Int
                      , from_id :: Maybe Int
                      , read_state :: Int 
                      , body :: Maybe T.Text 
                      , text :: Maybe T.Text 
                      , random_id :: Maybe Int 
                      , title :: Maybe T.Text 
                      , ref :: Maybe T.Text 
                      , ref_source :: Maybe T.Text 
                      , attachments :: Maybe [Attachments] 
                      , keyboard :: Maybe Keyboard 
                      } deriving (Show , Generic) 
instance FromJSON PrivateMessage 
instance ToJSON PrivateMessage 

data Attachments = Attachments
                   { _type :: T.Text 
                   , _acces_key :: Maybe T.Text 
                   } deriving (Show , Generic)
instance FromJSON Attachments where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}
instance ToJSON Attachments where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}   
      

data ClientInfo = ClientInfo 
                  { button_actions :: [T.Text ]
                  , keyboard :: Bool 
                  , inline_keyboard :: Bool 
                  , carousel :: Bool 
                  , lang_id :: Int
                  } deriving (Show , Generic)
instance FromJSON  ClientInfo 
instance ToJSON ClientInfo

data Keyboard = Keyboard
                { one_time :: Bool
                , buttons :: [[Buttons]]
                , inline :: Bool 
                } deriving (Show , Generic)
instance FromJSON  Keyboard
instance ToJSON Keyboard 

data Buttons = Buttons 
               { action :: Action
               , color :: Maybe T.Text 
               } deriving (Show , Generic)
instance FromJSON  Buttons     
instance ToJSON  Buttons

data Action = Action 
              { _type :: T.Text 
              , _label :: T.Text 
              , _payload :: T.Text 
              } deriving (Show , Generic)
instance FromJSON Action where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}
instance ToJSON Action where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}             

data MessageEvent = MessageEvent
                    { user_id :: Maybe Int 
                    , peer_id :: Maybe Int 
                    , event_id :: T.Text
                    , payload :: Maybe T.Text 
                    , conversation_message_id :: Int
                    } deriving (Show , Generic)
instance FromJSON MessageEvent 
instance ToJSON MessageEvent 

data Photo = Photo
             { id :: Int 
             , album_id :: Int
             , owner_id :: Int
             , user_id :: Maybe Int
             , text :: Maybe T.Text 
             , date :: Int 
             , sizes :: Maybe [Sizes] 
             , width :: Int
             , height :: Int
             } deriving (Show , Generic)
instance FromJSON Photo
instance ToJSON Photo     

data Sizes = Sizes
             { _rl_ :: T.Text 
             , _width :: Int
             , _height :: Int
             , _type :: T.Text 
             } deriving (Show , Generic)
instance FromJSON Sizes where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}
instance ToJSON Sizes where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}  

data Audio = Audio
             { id :: Int
             , owner_id :: Int
             , artist :: Maybe T.Text 
             , title :: Maybe T.Text 
             , duration :: Maybe Int
             , url :: Maybe T.Text 
             , date :: Int
             } deriving (Show , Generic)
instance FromJSON Audio
instance ToJSON Audio
             
data Video = Video
             { id :: Int
             , owner_id :: Int
             , title :: Maybe T.Text 
             , description :: Maybe T.Text 
             , duration :: Maybe Int
             , date :: Int
             , access_key :: Maybe T.Text 
             } deriving (Show , Generic)       
instance FromJSON Video
instance ToJSON Video

data WallPost = WallPost
                { id :: Int
                , owner_id :: Int
                , from_id :: Int
                , created_by :: Int
                , date :: Int
                , text :: T.Text 
                , post_type :: T.Text 
                , attachments :: [Attachments]
                } deriving (Show, Generic) 
instance FromJSON WallPost
instance ToJSON WallPost
            