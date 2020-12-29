{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}
module VkAPI where

import qualified Data.Text as T
import Prelude hiding ( id )
import GHC.Generics (Generic) 
import Data.Aeson ( genericParseJSON
                  , genericToJSON
                  , FromJSON (parseJSON)
                  , ToJSON (toJSON)
                  , defaultOptions
                  , Options (..) )

data VkResponse = VkRespounse 
                  { ts :: T.Text 
                  , updates :: [Update]
                  } deriving (Show , Generic)
instance FromJSON VkResponse
instance ToJSON VkResponse

data Update = Update
              { type_ :: T.Text 
              , object_ :: Object 
              , group_id_ :: Int 
              } deriving (Show, Generic)
instance FromJSON Update where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}
instance ToJSON Update where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}        

data Object = Object
              { message_new :: Maybe MessageNew
              , message_reply :: Maybe PrivateMessage
              , message_event :: Maybe MessageEvent
              , photo_new :: Maybe Photo 
          --    , photo_comment_new :: Maybe CommentPhoto
              , audio_new :: Maybe Audio 
              , video_new :: Maybe Video
              , wall_post_new :: Maybe WallPostNew
        --      , video_comment_new :: Maybe CommentVideo 
              } deriving (Show , Generic)
instance FromJSON  Object 
instance ToJSON Object

data MessageNew = MessageNew
                  { message :: PrivateMessage
                  , client_info :: Maybe ClientInfo
                  } deriving (Show , Generic)
instance FromJSON MessageNew 
instance ToJSON MessageNew

data PrivateMessage = PrivateMessage
                      { id :: Int 
                      , date :: Int 
                      , peer_id :: Int 
                      , from_id :: Int 
                      , text :: T.Text 
                      , random_id :: Int 
                      , ref :: Maybe T.Text 
                      , ref_source :: Maybe T.Text 
                      , attachments :: Maybe [Attachments] 
                      , keyboard :: Maybe Keyboard 
                      } deriving (Show , Generic) 
instance FromJSON PrivateMessage 
instance ToJSON PrivateMessage 

data Attachments = Attachments
                   { type_ :: T.Text 
                   , asses_key_ :: Maybe T.Text 
                   } deriving (Show , Generic)
instance FromJSON  Attachments
instance ToJSON Attachments

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
              { type_ :: T.Text 
              , label_ :: T.Text 
              , payload_ :: T.Text 
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
             { url_ :: T.Text 
             , width_ :: Int
             , height_ :: Int
             , type_ :: T.Text 
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

data WallPostNew = WallPostNew
                { id :: Int
                , owner_id :: Int
                , from_id :: Int
                , created_by :: Int
                , date :: Int
                , text :: T.Text 
                , post_type :: T.Text 
                , attachments :: [Attachments]
                } deriving (Show, Generic) 
instance FromJSON WallPostNew
instance ToJSON WallPostNew
            