{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Home where


import qualified Data.Double.Conversion.Text as Conversion
import           Instana.SDK.SDK             (InstanaContext)
import qualified Instana.SDK.SDK             as InstanaSDK

import           Import


getHomeR :: Handler Html
getHomeR = do
  App { instana } <- getYesod

  allProducts <- runDB $ getAllProducts instana
  result <- defaultLayout $ do
    aDomId <- newIdent
    setTitle "Stan's Monad Shop"
    $(widgetFile "homepage")
  InstanaSDK.addRegisteredDataAt instana "http.status" (200 :: Int)
  return result


getAllProducts :: InstanaContext -> DB [Entity Product]
getAllProducts instana =
  InstanaSDK.withExit instana "postgres" $ selectList [] [Asc ProductId]

