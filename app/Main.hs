{-# LANGUAGE OverloadedStrings #-}

module Main where

import VkAPI
import Data.Aeson (eitherDecode,  genericParseJSON
                  , genericToJSON
                  , FromJSON (parseJSON)
                  , ToJSON (toJSON)
                  , defaultOptions
                  , Options (..) )
import Network.HTTP.Simple
    ( Request, parseRequest_, Response, getResponseBody, httpLBS )
import qualified Data.ByteString.Lazy.Char8 as LBC    

main :: IO ()
main = do
    update <- httpLBS requestWith
    print update
    print $ getDecodeUpdate update

getDecodeUpdate :: Response LBC.ByteString -> VkUpdates    -- parse response from JSON
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
key1 = "fa47622c0f4fd0252148631d10183de068a8cbb5"
ts1 :: String
ts1 = "58"