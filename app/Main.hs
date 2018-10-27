{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class
import Data.ByteString.Lazy as L
import qualified Data.Text as Text
import Data.Proxy
import Lucid
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid
import Servant.Multipart

import Debug.Trace

import Pdf

type AusterityApi = "home" :> Get '[HTML] (Html ())
               :<|> "import" :> MultipartForm Tmp (MultipartData Tmp) :> Post '[HTML] (Html ())
               :<|> "static" :> Raw

home :: Html ()
home = do
    doctypehtml_ $ do
        html_ $ do
            head_ $ do
                title_ "Austerity"
            body_ $ do
                p_ "Welcome to Austerity"
                form_ [action_ "import", method_ "post", enctype_ "multipart/form-data"] $ do
                    input_ [type_ "file", name_ "file"]
                    input_ [type_ "submit", value_ "Import"]

doubleText = Text.pack . show . floor

renderTextBox :: Double -> TextBox -> Html ()
renderTextBox pageHeight (TextBox (Rect l t _ h) text) = do
    span_ [class_ "textbox", style_ ("left: " <> doubleText l <> "px; top: " <> doubleText (pageHeight - t) <> "px; font-size: " <> doubleText h <> "px;")] (toHtml text)

renderPage :: TextPage -> Html ()
renderPage (TextPage (Rect _ _ width height) bs) = do
    div_ [class_ "page", style_ ("width: " <> doubleText width <> "px; height: " <> doubleText height <> "px;")] $ do
        mapM_ (renderTextBox height) bs

renderPdf :: [TextPage] -> Html ()
renderPdf ps =  mapM_ renderPage ps

import' :: MultipartData Tmp -> Handler (Html ())
import' md = do
    pdfs <- case lookupFile "file" md of
            Just f -> do
                tp <- liftIO $ extractText (fdPayload f)
                return $ renderPdf (scaleTextPage 1.33333 <$> tp)
            Nothing -> return $ p_ "File not found"
    
    return $ doctypehtml_ $ do
                html_ $ do
                    head_ $ do
                        title_ "Austerity"
                        link_ [type_ "text/css", rel_ "stylesheet", href_ "static/site.css"]
                body_ $ do
                    pdfs

server :: Server AusterityApi
server = return home
    :<|> import'
    :<|> serveDirectoryFileServer "C:\\Users\\micah\\Source\\Austerity\\src\\Static"

main :: IO ()
main = run 8080 (serve (Proxy :: Proxy AusterityApi) server)
