{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.Test where

import Control.Monad.IO.Class
import Data.ByteString.Lazy as L
import Data.Text (Text)
import Data.Text.Encoding as TE
import qualified Data.Text as Text
import Text.Read
import Data.Proxy
import Lucid
import Network.Wai.Handler.Warp
import Servant
import Servant.HTML.Lucid

import Data.Maybe (isNothing)

import GHC.Generics

import Data.Time
import Data.Time.Format

import Data.Functor.Identity
import Data.Functor.Const

import Web.FormUrlEncoded

import Pdf.Content.Processor

import Debug.Trace

import Pdf
import Servant.JS

import Data.Aeson

import Servant.TS
import qualified Data.Text.IO as TIO

import Data.Typeable

data TestUnion = UnionA { _a :: Int, _b :: Int }
               | UnionB Text Text
    deriving (Generic, Typeable, ToJSON, TsTypeable)

type InnerApi = "receipts" :> Get '[JSON] [FullReceipt]
           :<|> "receipt" :> Capture "x" Int :> Get '[JSON] Int
           :<|> "receipts" :> "query" :> QueryParam "x" Int :> QueryParam "y" Int :> Post '[JSON] Int
           :<|> "receiptt" :> Capture "x" Text :> QueryParam "x" (Maybe Text) :> Get '[JSON] Int
           :<|> "receipt" :> "body" :> ReqBody '[JSON] FullReceipt :> Get '[JSON] Int
           :<|> "union" :> Get '[JSON] TestUnion

handler :: Maybe (FullReceipt' Identity) -> Handler Int
handler _ = return 7

h__ :: Server InnerApi
h__ = return undefined
 :<|> return undefined
 :<|> return undefined
 :<|> return undefined
 :<|> return undefined
 :<|> return undefined

h = serve (Proxy :: Proxy InnerApi) h__

g = do
    let t = tsForAPI (Proxy :: Proxy InnerApi)
    TIO.writeFile "C:/Users/Micah/Source/Austerity/build/Endpoints.ts" t

type AusterityHome = "home" :> Get '[HTML] (Html ())
type AusterityReceiptsNewGet = "receipts" :> "new" :> Get '[HTML] (Html ())
type AusterityReceiptsNewPost = "receipts" :> "new" :> ReqBody '[FormUrlEncoded] FullReceiptForm :> Post '[HTML] (Html ())

type AusterityJSTest = "receipts" :> "new" :> ReqBody '[JSON] FullReceiptForm :> Post '[JSON] FullReceiptError

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
    , items = []
    , amount = Const ("", Nothing)
    }

type family Props a where
    Props FullReceipt = FullReceiptProps

data FullReceiptProps = FullReceiptProps
    { vendors :: [VendorProps]
    } deriving (Generic)
 
deriving instance ToJSON FullReceiptProps

data VendorProps = VendorProps
    { vendorId :: Text
    , vendorName :: Text
    } deriving (Generic)

deriving instance ToJSON VendorProps
    
dateFormat :: String
dateFormat = "%m/%d/%Y %I:%M %p"

validateFullReceipt :: FullReceiptForm -> Either FullReceiptError FullReceipt
validateFullReceipt form = let mdate = parseTimeM False defaultTimeLocale dateFormat (Text.unpack . getConst . date $ form) :: Maybe LocalTime
                               mvendor = readMaybe (Text.unpack . getConst . vendor $ form) :: Maybe Int
                               mamount = readMaybe (Text.unpack . getConst . amount $ form) :: Maybe Double
                               receipt = FullReceipt' <$> (Identity <$> mdate) <*> (Identity <$> mvendor) <*> (Just [Identity 1, Identity 2]) <*> (Identity <$> mamount)
                           in  trace (show form) $ case receipt of
                               Just r -> Right r
                               Nothing -> Left $ FullReceipt'
                                            { date = Const (getConst $ date form, maybe (Just "The date must be the format 01/01/2000 12:00 AM") (const Nothing) mdate)
                                            , vendor = Const (getConst $ vendor form, maybe (Just "You must choose a vendor") (const Nothing) mvendor)
                                            , items = [Const ("1", Nothing)]
                                            , amount = Const (getConst $ amount form, maybe (Just "Must be in the format 100.00") (const Nothing) mamount)
                                            } 

data FullReceipt' a = FullReceipt'
    { date :: a LocalTime
    , vendor :: a Int
    , items :: [a Int]
    , amount :: a Double
    }

type FullReceipt = FullReceipt' Identity
deriving instance Show FullReceipt
deriving instance Generic FullReceipt
deriving instance TsTypeable FullReceipt
deriving instance ToJSON FullReceipt
deriving instance FromJSON FullReceipt

{- The type to be marshalled from the body of an HTTP request -}
type FullReceiptForm = FullReceipt' (Const Text)
deriving instance Show FullReceiptForm
deriving instance Generic FullReceiptForm
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