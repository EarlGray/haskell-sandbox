{-# LANGUAGE OverloadedStrings #-}

module HPaste (
  hpaste
) where

import Data.Text (Text, unpack, pack, append)
import qualified Data.Text.Lazy as TL
import Data.Maybe
import Data.Char (chr)
import Control.Monad (msum, when, replicateM)
import Control.Monad.Trans (liftIO)
import Control.Applicative ((<$>))
import System.Random
import System.Directory (doesFileExist, doesDirectoryExist, createDirectory)
import System.FilePath (combine)

import Happstack.Server
import qualified System.IO as IO

import Text.Blaze.Html5 ((!), Html, toHtml)
import Text.Blaze.Html5.Attributes (href)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

dbDir = "./hpaste"

formTitleName = "title"
formPasteName = "paste"

divclass :: Text -> Html -> Html
divclass clss = H.div ! A.class_  (H.toValue clss)

ahref :: Text -> Html -> Html
ahref href = H.a ! A.href (H.toValue href)

htmlError :: Text -> Html
htmlError msg = divclass "error" $ toHtml msg

hpaste :: Text -> (Html -> Response) -> ServerPart Response
hpaste hp templ = msum [ 
    path $ \p -> case p of
                  "list" -> listHPaste hp templ
                  _ -> getHPaste hp p templ,
    newHPaste hp templ,
    postHPaste hp templ ]


newHPaste :: Text -> (Html -> Response) -> ServerPart Response
newHPaste hp template = do
    method GET
    ok $ template $ 
      H.div ! A.class_ "hpaste" $ do
        H.pre (toHtml hp) ; H.hr
        hpasteForm hp

getHPaste :: Text -> String -> (Html -> Response) -> ServerPart Response
getHPaste hp url template = do
    let fpath = combine dbDir url
    exists <- liftIO $ doesFileExist fpath
    if (not exists) then do
      liftIO $ putStrLn $ "GET failed: " ++ url
      notFound $ template $ htmlError "Paste not found"
    else do
      txt <- liftIO $ readFile fpath
      liftIO $ putStrLn $ "GET: " ++ url
      ok $ template $ H.textarea ! A.readonly "1" ! A.rows "15" ! A.cols "80" $ toHtml $ pack txt

listHPaste :: Text -> (Html -> Response) -> ServerPart Response
listHPaste hp templ = do
    liftIO $ putStrLn "LIST"
    ok $ templ $ H.h3 "------ TODO: list -----"

postHPaste :: Text -> (Html -> Response) -> ServerPart Response
postHPaste hp template = do
    method POST
    title <- lookText formTitleName
    txt <- lookText formPasteName
    url <- liftIO $ savePaste $ pack . TL.unpack $ txt

    ok $ template $ H.div $ do
                     H.h4 "HPasted!"; H.hr
                     H.div $ ahref (pack $ (unpack hp) ++ "/" ++ url) (H.toHtml url)

hpasteForm :: Text -> Html
hpasteForm action = H.form ! A.id "hpaste" ! 
                      A.action (H.toValue action) ! 
                      A.enctype "multipart/form-data" ! 
                      A.method "POST" $ H.fieldset $ do
    H.legend "paste here"
    H.label ! A.for "title" $ "Title: "
    H.input ! A.type_ "text" ! A.id "title" ! A.name (H.toValue formTitleName)
    H.br
    H.textarea ! A.rows "15" ! A.cols "80" ! A.id "paste" ! A.name (H.toValue formPasteName) $ ""
    H.br
    H.input ! A.type_ "submit" ! A.value "Submit"
    H.input ! A.type_ "reset" ! A.value "Reset"

savePaste :: Text -> IO String
savePaste txt = do
    url <- randomURL
    let fpath = combine dbDir url
    exists <- doesFileExist fpath
    if exists then 
      savePaste txt  -- try to generate another URL
    else do
      dbexists <- doesDirectoryExist dbDir
      when (not dbexists) $ createDirectory dbDir
      writeFile fpath (unpack txt)
      liftIO $ putStrLn $ "POST: " ++ url
      return url

randomURL :: IO String
randomURL = map (abc !!) <$> (replicateM 8 $ randomRIO (0, length abc - 1))
    where abc = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

{-
loadPaste :: Text -> IO (Maybe Text)
loadPaste url = do
    let fpath = combine dbDir (unpack url)
    exists <- doesFileExist fpath
    if (not exists) then 
      return Nothing
    else do
      txt <- readFile fpath
      return $ Just $ pack txt
 - -}
