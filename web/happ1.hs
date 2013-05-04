{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>), optional)
import Data.Text (Text)
import Data.Maybe (fromMaybe)

import Happstack.Lite

import Text.Blaze.Html5 ((!), Html, toHtml)
import Text.Blaze.Html5.Attributes (href)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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
    H.h1 "Hello!"
    H.div ! A.class_ "main" $ do
      H.p "This is a test Happstack page created using Blaze"
      H.p "We are proud to show you the following:"
      H.ul $ do
        H.li $ H.a ! href "/echo/secret%20message"   $ "echo"
        H.li $ H.a ! href "/query?foo=bar"           $ "query parameters" 
        H.li $ H.a ! href "/form"                    $ "a form example"
        H.li $ H.a ! href "/fortune"                 $ "fortune cookies"
        H.li $ H.a ! href "/files"                   $ "files serving"
        H.li $ H.a ! href "/upload"                  $ "files upload"

echo :: ServerPart Response
echo = path $ \(msg :: String) ->
    ok $ template "echo" $ do
      H.p $ "echo: " >> toHtml msg
      H.p "----------------------"

queryParams :: ServerPart Response
queryParams = do
    mFoo <- optional $ lookText "foo"
    ok $ template "query params" $ do
      H.p $ "foo = " >> toHtml (show mFoo)
      H.hr

formPage :: ServerPart Response
formPage = msum [ viewForm, processForm ]
  where
    viewForm :: ServerPart Response
    viewForm = do
      method GET
      ok $ template "form" $
        H.form ! A.action "/form" ! A.enctype "multipart/form-data" ! A.method "POST" $ do
          H.label ! A.for "msg" $ "Say something clever: "
          H.input ! A.type_ "text" ! A.id "msg" ! A.name "msg"
          H.input ! A.type_ "submit" ! A.value "Say it!"

    processForm :: ServerPart Response
    processForm = do
      method POST
      msg <- lookText "msg"
      ok $ template "form" $ do
        H.p "You said; "
        H.p (toHtml msg)

fileServing :: ServerPart Response
fileServing = 
  serveDirectory EnableBrowsing ["index.htm", "index.html"] "."

fortune = echo
upload = echo

page404 :: String -> ServerPart Response
page404 p = notFound $ template "page not found" $ do
    divclass "error" $ toHtml p
    divclass "error" $ "It looks like that page does not exist"
  

myApp :: ServerPart Response
myApp = msum [
    dir "echo"      $ echo,
    dir "query"     $ queryParams,
    dir "form"      $ formPage,
    dir "fortune"   $ fortune,
    dir "files"     $ fileServing,
    dir "upload"    $ upload,
    path $ \(msg :: String) ->
      if null msg
      then homePage
      else page404 msg ]

main :: IO ()
main = do
  putStrLn "Server started"
  serve Nothing myApp
