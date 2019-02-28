{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AusterityApi where

import Data.Functor.Identity
import Data.Functor.Const
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Data.Typeable
import GHC.Generics

import Data.Aeson
import Servant
import Servant.TS

type AusterityApi = "receipts" :> Get '[JSON] [FullReceipt]
               :<|> "receipt" :> "new" :> ReqBody '[JSON] FullReceiptForm :> Post '[JSON] FullReceiptForm
               :<|> "receipt" :> Capture "id" Int :> Get '[JSON] FullReceipt
               :<|> "polymorphic1" :> Get '[JSON] (BasicPolymorphicType Int)
               :<|> "polymorphic2" :> Get '[JSON] (BasicPolymorphicType Text)

data FullReceipt' a = FullReceipt'
    { date :: a LocalTime
    , vendor :: a Int
    , items :: [a Int]
    , amount :: a Double
    }

data BasicPolymorphicType a = BasicPolymorphicType a
    deriving (Generic, Typeable, TsTypeable)

type FullReceipt = FullReceipt' Identity
deriving instance Show FullReceipt
deriving instance Generic FullReceipt
deriving instance TsTypeable FullReceipt
deriving instance ToJSON FullReceipt
deriving instance FromJSON FullReceipt

type FullReceiptForm = FullReceipt' (Const Text)
deriving instance Show FullReceiptForm
deriving instance Generic FullReceiptForm
deriving instance Typeable FullReceiptForm
deriving instance TsTypeable FullReceiptForm
deriving instance ToJSON FullReceiptForm

sampleForm = FullReceipt' (Const "date") (Const "vendor") [] (Const "amount") :: FullReceiptForm

newtype FullReceiptForm' = FullReceiptForm' { unFullReceiptForm :: FullReceiptForm }
    deriving (Show, Generic, Typeable, TsTypeable, ToJSON)