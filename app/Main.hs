{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Control.Monad.IO.Class
import Data.ByteString.Lazy as L
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Proxy
import Lucid
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid

import Data.Maybe (isNothing)

import GHC.Generics

import qualified Beam as Beam
import qualified Database.Beam as B
import qualified Database.Beam.Sqlite as BS
import qualified Database.SQLite.Simple as SS

import Data.Time

import Data.Functor.Identity
import Data.Functor.Const

import Web.FormUrlEncoded

import Pdf.Content.Processor

import qualified Data.ByteString as BS

import Pdf

type AusterityHome = "home" :> Get '[HTML] (Html ())
type AusterityReceiptsNewGet = "receipts" :> "new" :> Get '[HTML] (Html ())
type AusterityReceiptsNewPost = "receipts" :> "new" :> ReqBody '[FormUrlEncoded] FullReceiptForm :> Post '[HTML] (Html ())

type AusterityApi = AusterityHome
               :<|> AusterityReceiptsNewGet
               :<|> AusterityReceiptsNewPost
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

defaultFullReceiptError :: FullReceiptError
defaultFullReceiptError = FullReceipt' 
    { date = Const ("", Nothing)
    , vendor = Const ("", Nothing)
    , amount = Const ("", Nothing)
    }

newReceipt :: SS.Connection -> Handler (Html ())
newReceipt conn = do
    vendors <- liftIO $ BS.runBeamSqlite conn $ B.runSelectReturningList $ B.select $ B.orderBy_ (\v -> B.asc_ $ Beam._vendor_Name v) $ B.all_ (Beam._vendors Beam.austerityDb)
    return $ do
        doctypehtml_ $ do
            html_ $ do
                head_ $ do
                    title_ "Austerity"
                    link_ [type_ "text/css", rel_ "stylesheet", href_ "/static/bootstrap.min.css"]
                    link_ [type_ "text/css", rel_ "stylesheet", href_ "/static/site.css"]
                body_ $ do
                    p_ "Create new Receipt"
                    fullReceiptForm defaultFullReceiptError vendors

fullReceiptForm :: FullReceiptError -> [Beam.Vendor] -> Html ()
fullReceiptForm receipt vendors = do
    form_ [class_ "form", action_ $ "/" <> toUrlPiece (safeLink (Proxy :: Proxy AusterityApi) (Proxy :: Proxy AusterityReceiptsNewGet)), method_ "post"] $ do
        div_ [class_ "form-group"] $ do
            label_ [for_ "date"] "Date"
            let isValid = isNothing . snd . getConst . date $ receipt
            input_ [type_ "text", class_ ("form-control" <> (if isValid then "" else " is-invalid")), name_ "date", id_ "date", value_ (fst . getConst . date $ receipt)]
            makeError $ date receipt
        div_ [class_ "form-group"] $ do
            label_ [for_ "vendor"] "Vendor"
            let isValid = isNothing . snd . getConst . date $ receipt
            select_ [name_ "vendor", class_ ("form-control" <> (if isValid then "" else " is-invalid")), id_ "vendor"] $ do
                mapM_ (\v -> let attribs = [value_ (Text.pack . show $ Beam._vendor_VendorId v)] 
                                 selected = if (Text.pack . show . Beam._vendor_VendorId $ v) == (fst . getConst . vendor $ receipt) then selected_ "selected" : attribs else attribs
                             in  option_ selected (toHtml $ Beam._vendor_Name v)) vendors
            makeError $ vendor receipt
        div_ [class_ "form-group"] $ do
            label_ [for_ "amount"] "Amount"
            let isValid = isNothing . snd . getConst . amount $ receipt
            input_ [type_ "text", class_ ("form-control" <> (if isValid then "" else " is-invalid")), name_ "amount", value_ (fst . getConst . amount $ receipt)]
            makeError $ amount receipt
        button_ [type_ "submit", class_ "btn btn-primary"] "Submit"

    where makeError :: Const (a, Maybe Text) b -> Html ()
          makeError f = case snd . getConst $ f of
              Nothing -> pure ()
              Just e -> div_ [class_ "invalid-feedback"] $ toHtml e

newReceiptPost :: SS.Connection -> FullReceiptForm -> Handler (Html ())
newReceiptPost conn r = do
    vendors <- liftIO $ BS.runBeamSqlite conn $ B.runSelectReturningList $ B.select $ B.orderBy_ (\v -> B.asc_ $ Beam._vendor_Name v) $ B.all_ (Beam._vendors Beam.austerityDb)
    return $ do
        doctypehtml_ $ do
            html_ $ do
                head_ $ do
                    title_ "Austerity"
                    link_ [type_ "text/css", rel_ "stylesheet", href_ "/static/bootstrap.min.css"]
                    link_ [type_ "text/css", rel_ "stylesheet", href_ "/static/site.css"]
                body_ $ do
                    p_ "Create new Receipt"
                    fullReceiptForm (validateFullReceipt r) vendors

validateFullReceipt :: FullReceiptForm -> FullReceiptError
validateFullReceipt form = FullReceipt'
    { date = Const (getConst $ date form, Just "a")
    , vendor = Const (getConst $ vendor form, Just "b")
    , amount = Const (getConst $ amount form, Just "c")
    } 

data FullReceipt' a = FullReceipt'
    { date :: a LocalTime
    , vendor :: a Int
    , amount :: a Double
    } deriving (Generic)

type FullReceipt = FullReceipt' Identity
deriving instance Show FullReceipt

{- The type to be marshalled from the body of an HTTP request -}
type FullReceiptForm = FullReceipt' (Const Text)
deriving instance Show FullReceiptForm
deriving instance FromForm FullReceiptForm

{-  -}
type FullReceiptError = FullReceipt' (Const (Text, Maybe Text))
deriving instance Show FullReceiptError 

instance (FromHttpApiData a) => FromHttpApiData (Const a b) where
    parseUrlPiece t = Const <$> parseUrlPiece t

doubleText = Text.pack . show

renderTextBox :: Double -> TextBox -> Html ()
renderTextBox pageHeight (TextBox (Rect l t w h) text) = do
    div_ [class_ "textbox", style_ ("left: " <> doubleText l <> "px; top: " <> doubleText (pageHeight - t) <> "px; font-size: " <> doubleText h <> "px; width: " <> doubleText w <> "px; height: " <> doubleText h <> "px;")] (toHtml text)

renderPage :: TextPage -> Html ()
renderPage (TextPage (Rect _ _ width height) bs) = do
    div_ [class_ "page", style_ ("width: " <> doubleText width <> "px; height: " <> doubleText height <> "px;")] $ do
        mapM_ (renderTextBox height) bs

renderPdf :: [TextPage] -> Html ()
renderPdf ps =  mapM_ renderPage ps

{-import' :: MultipartData Tmp -> Handler (Html ())
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
-}

server :: SS.Connection -> Server AusterityApi
server conn = return home
    :<|> newReceipt conn
    :<|> newReceiptPost conn 
    :<|> serveDirectoryFileServer "C:\\Users\\micah\\Source\\Austerity\\src\\Static"

main :: IO ()
main = do
    conn <- SS.open "C:/Users/Micah/Desktop/Austerity.db"
    run 8080 (serve (Proxy :: Proxy AusterityApi) (server conn))
