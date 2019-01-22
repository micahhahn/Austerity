{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Beam where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time

import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple

data AusterityDb f = AusterityDb
    { _vendors :: f (TableEntity VendorT) 
    , _receipts :: f (TableEntity ReceiptT)
    , _receiptItems :: f (TableEntity ReceiptItemT)
    } deriving Generic

instance Database be AusterityDb

austerityDb :: DatabaseSettings be AusterityDb
austerityDb = defaultDbSettings

data VendorT f = Vendor 
    { _vendor_VendorId :: C f Int
    , _vendor_Name :: C f Text
    , _vendor_Address :: C f Text
    , _vendor_Latitude :: C f Double
    , _vendor_Longitude :: C f Double
    } deriving (Generic, Beamable)

type Vendor = VendorT Identity
type VendorId = PrimaryKey VendorT Identity

deriving instance Show Vendor
deriving instance Show (PrimaryKey VendorT Identity)
deriving instance Eq Vendor
deriving instance Eq (PrimaryKey VendorT Identity)

instance Table VendorT where
    data PrimaryKey VendorT f = VendorId (C f Int) deriving (Generic, Beamable)
    primaryKey = VendorId . _vendor_VendorId

data ReceiptT f = Receipt
    { _receipt_ReceiptId :: C f Int
    , _receipt_Date :: C f LocalTime
    , _receipt_Vendor :: PrimaryKey VendorT f
    , _receipt_Amount :: C f Double
    } deriving (Generic, Beamable)

type Receipt = ReceiptT Identity
type ReceiptId = PrimaryKey ReceiptT Identity

deriving instance Show Receipt
deriving instance Show (PrimaryKey ReceiptT Identity)
deriving instance Eq Receipt
deriving instance Eq (PrimaryKey ReceiptT Identity)

instance Table ReceiptT where
    data PrimaryKey ReceiptT f = ReceiptId (C f Int) deriving (Generic, Beamable)
    primaryKey = ReceiptId . _receipt_ReceiptId

data ReceiptItemT f = ReceiptItem
    { _receiptItem_ReceiptItemId :: C f Int
    , _receiptItem_ReceiptId :: PrimaryKey ReceiptT f
    , _receiptItem_Name :: C f Text
    , _receiptItem_Price :: C f Text
    } deriving (Generic, Beamable)

type ReceiptItem = ReceiptItemT Identity
type ReceiptItemId = PrimaryKey ReceiptItemT Identity

deriving instance Show ReceiptItem
deriving instance Show (PrimaryKey ReceiptItemT Identity)
deriving instance Eq ReceiptItem
deriving instance Eq (PrimaryKey ReceiptItemT Identity)

instance Table ReceiptItemT where
    data PrimaryKey ReceiptItemT f = ReceiptItemId (C f Int) deriving (Generic, Beamable)
    primaryKey = ReceiptItemId . _receiptItem_ReceiptItemId

x :: IO ()
x = do
    conn <- open "C:/Users/Micah/Desktop/Austerity.db"
    runBeamSqliteDebug putStrLn {- debugging output -} conn $ do
        transactions <- runSelectReturningList $ select (all_ (_vendors austerityDb))
        mapM_ (liftIO . putStrLn . show) transactions