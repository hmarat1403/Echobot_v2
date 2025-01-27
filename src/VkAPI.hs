{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, DuplicateRecordFields, OverloadedStrings, GADTs #-}
module VkAPI where

import qualified Data.Text as T
import Prelude hiding ( id )
import GHC.Generics (Generic) 
import Data.Aeson (Value(Null), (.:?), (.:), withObject, eitherDecode,  genericParseJSON
                  , genericToJSON
                  , FromJSON (parseJSON)
                  , ToJSON (toJSON)
                  , defaultOptions
                  , Options (..) )
import Network.HTTP.Simple
    ( Request, parseRequest_, Response, getResponseBody )
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Aeson.Types (parseFail, emptyObject, Value(Object))




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
            "wall_reply_new" -> ObjWallReply <$> v .: "object"
            _ ->  parseFail "This message can't parsing yet"
        _group_id <- v .: "group_id"
        _event_id <- v .: "event_id"
        return Update{..}      

data Object = ObjMessageNew MessageNew
            | ObjMessageReply MessageReply 
            | ObjMessageEvent MessageEvent 
            | ObjPhoto Photo
            | ObjVideo Video
            | ObjAudio Audio
            | ObjWallPost WallPost
            | ObjWallReply WallReply
                deriving Show 

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
                      , title :: T.Text 
                      , ref :: Maybe T.Text 
                      , ref_source :: Maybe T.Text 
                      , attachments :: Maybe [Attachments] 
                      , keyboard :: Maybe Keyboard 
                      } deriving (Show , Generic) 
instance FromJSON PrivateMessage 
--instance ToJSON PrivateMessage 

data Attachments = Attachments
                   { _type :: T.Text 
                   , _object :: AttachObject
                   , _access_key :: Maybe T.Text 
                   } deriving Show 
instance FromJSON Attachments where
    parseJSON = withObject "attachment" $ \v -> do
        _type <- v .: "type" 
        _object <- case _type :: T.Text of 
            "photo" -> AttachPhoto <$> v .: "photo"
            "video" -> AttachVideo <$> v .: "video"
            "audio" -> AttachAudio <$> v .: "audio"
            "doc" -> AttachDocument <$> v .: "doc"
            "link" -> AttachLink <$> v .: "link"
            "market" -> AttachMarket <$> v .: "market"
            "market_album" -> AttachMarketAlbum <$> v .: "market_album"
            "wall" -> AttachWallPost <$> v .: "wall"
            "wall_reply" -> AttachWallReply <$> v .: "wall_reply"
            "sticker" -> AttachSticker <$> v .: "sticker"
            "gift" -> AttachWallPost <$> v .: "gift"
        _access_key <- v .:? "access_key"
        return Attachments{..}   

data AttachObject = AttachPhoto Photo 
                  | AttachAudio Audio    
                  | AttachVideo Video
                  | AttachLink Link
                  | AttachDocument Document
                  | AttachMarket Market
                  | AttachMarketAlbum MarketAlbum
                  | AttachGift Gift
                  | AttachWallPost WallPost
                  | AttachWallReply WallReply
                  | AttachSticker Sticker
                    deriving Show 

data ClientInfo = ClientInfo 
                  { button_actions :: [T.Text]
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
             , text :: T.Text 
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
             , artist :: T.Text 
             , title :: T.Text 
             , duration :: Int
             , url :: T.Text 
             , date :: Int
             } deriving (Show , Generic)
instance FromJSON Audio
instance ToJSON Audio
             
data Video = Video
             { id :: Int
             , owner_id :: Int
             , title :: T.Text 
             , description :: T.Text 
             , duration :: Int
             , date :: Int
             , access_key :: Maybe T.Text 
             } deriving (Show , Generic)       
instance FromJSON Video
instance ToJSON Video

data WallPost = WallPost
                { id :: Int
                , owner_id :: Maybe Int
                , to_id :: Maybe Int 
                , from_id :: Int
                , created_by :: Int
                , date :: Int
                , text :: T.Text 
                , post_type :: T.Text 
                , attachments :: [Attachments]
                } deriving (Show, Generic) 
instance FromJSON WallPost
--instance ToJSON WallPost
            
data Document = Document 
               { _id :: Int
               , _owner_id :: Maybe Int
               , _title :: T.Text 
               , _size :: Int
               , _ext :: Maybe T.Text 
               , _url :: T.Text 
               , _date :: Int
               , _type :: Int
               } deriving (Show, Generic)   
instance FromJSON Document where
    parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}
instance ToJSON Document where
    toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}                 

data Link = Link
           { url :: T.Text 
           , title :: T.Text 
           , description :: Maybe T.Text 
           , photo :: Maybe Photo
           , image_src :: T.Text 
           , preview_page :: T.Text 
           , preview_url :: T.Text 
           } deriving (Show , Generic)
instance FromJSON Link
instance ToJSON Link

data WallReply = WallReply
                 { id :: Int
                 , from_id :: Int
                 , post_id :: Maybe Int
                 , post_owner_id :: Maybe Int 
                 , owner_id :: Maybe Int
                 , date :: Int 
                 , text :: T.Text 
                 , attachments :: Maybe Attachments
                 } deriving (Show , Generic)
instance FromJSON WallReply
--instance ToJSON WallReply

data Sticker = Sticker
               { product_id :: Int
               , stticker_id :: Int
               , images :: [Image]
               , images_with_background :: [Image]
               , animation_url :: Maybe T.Text 
               , is_allowed :: Bool 
               } deriving (Show , Generic)
instance FromJSON  Sticker
instance ToJSON Sticker

data Image = Image 
             { url :: T.Text 
             , width :: Int
             , height :: Int
             } deriving (Show , Generic)
instance FromJSON Image
instance ToJSON Image       

data Gift = Gift
            { id :: Int
            , thumb_256 :: T.Text 
            , thumb_96 :: T.Text 
            , thumb_48 :: T.Text
            } deriving (Show , Generic)
instance FromJSON  Gift
instance ToJSON Gift            

data Market = Market 
              { id :: Int
              , owner_id :: Int
              , title :: Maybe T.Text 
              , description :: Maybe T.Text 
              , date :: Int
              } deriving (Show , Generic)
instance FromJSON  Market
instance ToJSON Market          

data MarketAlbum = MarketAlbum
                   { id :: Int
                   , owner_id :: Int
                   , title :: T.Text 
                   , photo :: Maybe Photo
                   , count :: Int
                   , updated_time :: Int 
                   } deriving (Show , Generic)
instance FromJSON  MarketAlbum
instance ToJSON MarketAlbum                   