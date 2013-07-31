{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Data.Text (pack)
import qualified Data.Map as Map
import Control.Monad (forM, forM_, join, sequence, msum)
import Control.Applicative ((<$>), optional)
import Data.Text (Text)
import Data.Maybe (fromMaybe)

import qualified Happstack.Server as Srv
import Happstack.Server (dirs, Request(..), Response, HeaderPair(..), ok, notFound, toResponse, ServerPart(..), method, path)
import qualified Happstack.Server as Srv

import Text.Blaze.Html5 ((!), Html, toHtml)
import Text.Blaze.Html5.Attributes (href)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import HPaste

template :: Text -> Html -> Response
template title body = toResponse $ H.docTypeHtml $ 
    H.html $ do
      H.head $ do
        H.title (toHtml title)
        linkCss "/files/style.css"
        H.meta ! A.charset "UTF-8"
      H.body $ do
        body
        H.hr
        H.a ! A.href "/" $ "home"
        H.em " (c) dmytrish, 2013"

linkCss filepath = H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href filepath
divclass clss = H.div ! A.class_ clss

homePage :: ServerPart Response
homePage = ok $ template "home page" $ do
    H.h2 "Hello!"
    divclass "main" $ do
      H.p "This is a test Happstack page created using Blaze"
      H.p "We are proud to show you the following:"
      H.ul $ do
        H.li $ H.a ! href "/echo/secret%20message"   $ "echo"
        H.li $ H.a ! href "/query?foo=bar"           $ "query parameters" 
        H.li $ H.a ! href "/form"                    $ "a form example"
        H.li $ H.a ! href "/fortune"                 $ "fortune cookies"
        H.li $ H.a ! href "/you"                     $ "info about you"
        H.li $ H.a ! href "/files"                   $ "files serving"
        H.li $ H.a ! href "/upload"                  $ "files upload"
      H.p $ H.a ! A.href "/hpaste" $ "HPaste service"

echo :: ServerPart Response
echo = path $ \(msg :: String) ->
    ok $ template "echo" $ do
      H.p $ "echo: " >> toHtml msg
      H.p "----------------------"

queryParams :: ServerPart Response
queryParams = do
    mFoo <- optional $ Srv.lookText "foo"
    ok $ template "query params" $ do
      H.p $ "foo = " >> toHtml (show mFoo)
      H.hr

formPage :: ServerPart Response
formPage = msum [ viewForm, processForm ]
  where
    viewForm :: ServerPart Response
    viewForm = do
      method Srv.GET
      ok $ template "form" $
        H.form ! A.action "/form" ! A.enctype "multipart/form-data" ! A.method "POST" $ do
          H.label ! A.for "msg" $ "Say something clever: "
          H.input ! A.type_ "text" ! A.id "msg" ! A.name "msg"
          H.input ! A.type_ "submit" ! A.value "Say it!"

    processForm :: ServerPart Response
    processForm = do
      method Srv.POST
      msg <- Srv.lookText "msg"
      ok $ template "form" $ do
        H.p "You said; "
        H.p (toHtml msg)

fileServing :: ServerPart Response
fileServing = 
  Srv.serveDirectory Srv.EnableBrowsing ["index.htm", "index.html"] "."

fortune = echo
upload = echo

page404 :: String -> ServerPart Response
page404 p = notFound $ template "page not found" $ do
    divclass "error" $ toHtml p
    divclass "error" $ "It looks like that page does not exist"

prettyHeaders :: Srv.Headers -> [String]
prettyHeaders hdrs = map (showHeader . snd)  $ Map.toList hdrs
  where showHeader Srv.HeaderPair{ hName = name, hValue = val } = 
            show name ++ ": " ++ show val 
  
clientInfo :: ServerPart Response
clientInfo = do
    Request { rqPeer = (host, _), rqHeaders = hdrs } <- Srv.askRq
    ok $ template "About you" $ H.div $ do 
      H.p $ H.toHtml $ "Connection from " ++ show host
      H.p $ do H.toHtml ("Headers: " :: String)
               forM_ (prettyHeaders hdrs) $ \line -> do
                  H.br ; H.toHtml line

myApp :: ServerPart Response
myApp = msum [
    Srv.dir "echo"      $ echo,
    Srv.dir "query"     $ queryParams,
    Srv.dir "form"      $ formPage,
    Srv.dir "fortune"   $ fortune,
    Srv.dir "files"     $ fileServing,
    Srv.dir "you"       $ clientInfo,
    Srv.dir "hpaste"    $ hpaste "/hpaste" (template "hpaste"),
    Srv.dir "upload"    $ upload,
    path $ \(msg :: String) ->
      if null msg
      then homePage
      else page404 msg,
    dirs ""        $ homePage ]

main :: IO ()
main = do
  putStrLn "Server started"
  Srv.simpleHTTP Srv.nullConf myApp
