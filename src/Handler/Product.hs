{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Product where

import           Data.Aeson                  ((.=))
import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString.Lazy.Char8  as Char8
import qualified Data.Double.Conversion.Text as Conversion
import           Instana.SDK.SDK             (InstanaContext)
import qualified Instana.SDK.SDK             as InstanaSDK
import qualified Network.HTTP.Conduit        as HTTP

import           Import


getProductR :: Key Product -> Handler Html
getProductR key = do
  App { instana, appHttpManager } <- getYesod
  request <- waiRequest
  -- TODO Capture extra headers, helper function from SDK needed
  InstanaSDK.withHttpEntry instana request "haskell.wai.server" $ do
    p <- runDB $ getProductDetails instana key
    case p of
      Nothing -> do
        notFound
      Just details -> do
        recommendations <-
          liftIO $
            getRecommendations instana appHttpManager key
        result <- defaultLayout $ do
          $(widgetFile "product")
        InstanaSDK.addRegisteredDataAt instana "http.status" (200 :: Int)
        return result


getProductDetails ::
  InstanaContext ->
  Key Product ->
  DB (Maybe Product)
getProductDetails instana key =
  InstanaSDK.withExit instana "postgres" $ get key


getRecommendations ::
  InstanaContext ->
  HTTP.Manager ->
  Key Product ->
  IO Text
getRecommendations instana appHttpManager key = do
  requestBase <-
    HTTP.parseUrlThrow $ "http://localhost:83/recommendations/" ++ (show key)
  request <- InstanaSDK.addHttpTracingHeaders instana $
    requestBase
      { HTTP.requestHeaders =
        [ ("Accept", "application/json")
        , ("Content-Type", "application/json; charset=UTF-8'")
        ]
      }
  InstanaSDK.withExit instana "haskell.http.client" $ do
    InstanaSDK.addRegisteredData
      instana
      (Aeson.object [ "http" .=
        Aeson.object
          [ "method" .= ("GET" :: String)
          , "url"    .= ("/recommendations/{id}" :: String)
          ]
        ]
      )
    catch
      ( do
        response <- HTTP.httpLbs request appHttpManager
        InstanaSDK.addRegisteredDataAt instana "http.status" (200 :: Int)
        let
          body = HTTP.responseBody response
          result = pack $ Char8.unpack body
        return result
      )
      (\(_ :: SomeException) -> do
        InstanaSDK.incrementErrorCount instana
        InstanaSDK.addRegisteredDataAt instana "http.status" (500 :: Int)
        InstanaSDK.addRegisteredDataAt instana "http.error" ("Request failed" :: String)
        return "Recommendations could not be loaded."
      )

