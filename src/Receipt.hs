{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGuAGE OverloadedStrings #-}

module Receipt (

) where

import Data.Decimal
import Data.Hourglass
import Data.Text (Text)

data Vendor = Vendor
    { name :: Text
    , address :: Text
    } deriving (Show)

{- TODO: Sales Tax? -}
data Receipt = Receipt
    { vendor :: Vendor
    , date :: DateTime
    , items :: [ReceiptItem]
    } deriving (Show)

{- TODO: Include units and amounts -}
data ReceiptItem = ReceiptItem
    { name :: Text
    , price :: Decimal
    } deriving (Show)

greenbayMarketplace = Vendor { name = "Greenbay Marketplace", address = "3206 Broadway, Astoria, NY 11106" }

receipt1 = Receipt 
    { vendor = greenbayMarketplace
    , date = DateTime (Date 2018 November 9) (TimeOfDay 6 11 25 0)
    , items = 
        [ ReceiptItem { name = "Almond Dream Chocolate Non-Dairy", price = realFracToDecimal 2 4.99 }
        , ReceiptItem { name = "TINKYADA Brown Rice Pasta", price = realFracToDecimal 2 4.99 }
        , ReceiptItem { name = "Phillips Baby Bella Mushrooms", price = realFracToDecimal 2 2.99 }
        , ReceiptItem { name = "Produce (Cilantro, Parsley)", price = realFracToDecimal 2 1.98 }
        ]
    }

