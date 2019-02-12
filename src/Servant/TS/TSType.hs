{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Servant.TS (
    TSType(..)
) where

{- import Data.DList (DList) -}
import Data.Fixed (Fixed, HasResolution)
import Data.Functor.Compose (Compose)
import Data.Functor.Identity (Identity)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.IntSet (IntSet)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.Ratio (Ratio)
{- import Data.Scientific (Scientific) -}
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Data.Version (Version)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.Types (CTime)
import GHC.Generics
import Numeric.Natural (Natural)

import Servant.TS.Types

import Data.Aeson

data X = X { x1 :: Int, x2 :: Int }
       | Y Bool
    deriving (Generic, Typeable, TSType, ToJSON)

class TSType a where
    tsType :: Proxy a -> TypeContext Text
    default tsType :: (Generic a, TSDatatype (Rep a)) => Proxy a -> TypeContext Text
    tsType _ = tsDatatype (from (undefined :: a))

class TSDatatype a where
    tsDatatype :: a p -> TypeContext Text

instance (Datatype a, TSConstructor c) => TSDatatype (D1 a c) where
    tsDatatype c@(M1 r) = tsConstructor r False

class TSConstructor a where
    tsConstructor :: a p -> Bool -> TypeContext Text

instance (TSConstructor a, TSConstructor b) => TSConstructor (a :+: b) where
    tsConstructor (_ :: (a :+: b) f) _ = do
        l <- tsConstructor (undefined :: a f) True
        r <- tsConstructor (undefined :: b f) True
        return $ l <> " | " <> r
                                         
instance (Constructor a, TSSelector c) => TSConstructor (C1 a c) where
    tsConstructor c@(M1 r) isTagged = do
        s <- tsSelector r
        let interface = "interface " <> (Text.pack . conName $ c) <> "\n" <>
                        "{\n" <>
                        "\t" <> if isTagged then "tag: \"" <> (Text.pack . conName $ c) <> "\",\n" <>
                        "}"
        return $ (Text.pack . conName $ c)

class TSSelector a where
    tsSelector :: a p -> TypeContext [(Text, Text)]

instance (TSSelector a, TSSelector b) => TSSelector (a :*: b) where
    tsSelector (_ :: (a :*: b) f) = do
        l <- tsSelector (undefined :: a f)
        r <- tsSelector (undefined :: b f)
        return (l ++ r)

instance (Selector a, TSSelector c) => TSSelector (S1 a c) where
    tsSelector c@(M1 r) = return [(Text.pack . selName $ c, "")]

instance (TSType c) => TSSelector (K1 a c) where
    tsSelector _ = undefined

instance TSSelector U1 where
    tsSelector _ = undefined

instance TSType Bool where
    tsType _ = return "boolean"

instance TSType Ordering where
    tsType _ = return "string"

instance TSType () where
    tsType _ = return "Array<never>"

instance TSType Char where
    tsType _ = return "string"

instance TSType Double where
    tsType _ = return "number"

{-
instance TSType Number where
    tsType _ = "number"
-}

instance TSType Float where
    tsType _ = return "number"

instance (TSType a, Integral a) => TSType (Ratio a) where
    tsType _ = return "number"
    
instance (HasResolution a) => TSType (Fixed a) where
    tsType _ = return "number"

instance TSType Int where
    tsType _ = return "number"

instance TSType Integer where 
    tsType _ = return "number"

instance TSType Natural where
    tsType _ = return "number"

instance TSType Int8 where
    tsType _ = return "number"

instance TSType Int16 where
    tsType _ = return "number"

instance TSType Int32 where
    tsType _ = return "number"

instance TSType Int64 where
    tsType _ = return "number"

instance TSType Word where
    tsType _ = return "number"

instance TSType Word8 where
    tsType _ = return "number"

instance TSType Word16 where
    tsType _ = return "number"

instance TSType Word32 where
    tsType _ = return "number"

instance TSType Word64 where
    tsType _ = return "number"

instance TSType CTime where
    tsType _ = return "number"

instance TSType Text where
    tsType _ = return "string"

instance TSType LT.Text where
    tsType _ = return "string"

instance TSType Version where
    tsType _ = return "string"

instance (TSType a) => TSType (NonEmpty a) where
    tsType _ = do
        ts <- tsType (Proxy :: Proxy a)
        return $ "Array<" <> ts <> ">"

{-
instance TSType Scientific where
    tsType _ = "number"
-}

{-
instance (TSType a) => TSType (DList a) where
    tsType _ = "Array<" <> tsType (Proxy :: Proxy a) <> ">"
-}

instance (TSType a) => TSType (Identity a) where
    tsType _ = tsType (Proxy :: Proxy a)

{-
instance (TSType a, TSType b) => TSType (Compose f a b) where
    tsType _ = 
-}

{-
instance (TSType a, TSType b) => TSType (Product f a b) where
    tsType _ = 
-}

instance (TSType a) => TSType (Seq a) where
    tsType _ = do
        ts <- tsType (Proxy :: Proxy a)
        return $ "Array<" <> ts <> ">"

instance (TSType a) => TSType (Set a) where
    tsType _ = do
        ts <- tsType (Proxy :: Proxy a)
        return $ "Array<" <> ts <> ">"

instance TSType IntSet where
    tsType _ = do
        ts <- tsType (Proxy :: Proxy Int)
        return $ "Array<" <> ts <> ">"

{- instance TSType IntMap where -}

{- instance TSType Tree where -}

{- instance TSType UUID -}

instance (TSType a) => TSType (V.Vector a) where
    tsType _ = do
        ts <- tsType (Proxy :: Proxy a)
        return $ "Array<" <> ts <> ">"

{- instance VS.Vector -}

{- instance VP.Vector -}

{- instance VU.Vector -}

{- instance HashSet -}

{- instance HashMap -}

{- instance PM.Array -}

{- instance PM.SmallArray -}

{- instance PM.PrimArray -}

{- instance PM.UnliftedArray -}

{- instance ToJSON Day where -}

{- instance ToJSON TimeOfDay where -}

{- instance ToJSON LocalTime where -}

{- instance ToJSON ZonedTime where -}

{- instance ToJSON UTCTime where -}

{- instance ToJSON NominalDiffTime where -}

{- instance ToJSON DiffTime where -}

{- instance ToJSON a => ToJSON (Monoid.Dual a) where -}

{- instance ToJSON a => ToJSON (Monoid.First a) where -}

{- instance ToJSON a => ToJSON (Monoid.Last a) where -}

{- instance ToJSON a => ToJSON (Semigroup.Min a) where -}

{- instance ToJSON a => ToJSON (Semigroup.Max a) where -}

{- instance ToJSON a => ToJSON (Semigroup.First a) where -}

{- instance ToJSON a => ToJSON (Semigroup.Last a) where -}

{- instance ToJSON a => ToJSON (Semigroup.WrappedMonoid a) where -}

{- instance ToJSON a => ToJSON (Semigroup.Option a) where -}

{- instance ToJSON (Proxy a) where -}

{- instance ToJSON b => ToJSON (Tagged a b) where -}

{- instance (ToJSON a, ToJSON b) => ToJSON (a, b) where -}

{- instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (a, b, c) where -}

{- instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON (a, b, c, d) where -}

{- instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e) => ToJSON (a, b, c, d, e) where -}

{- instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f) => ToJSON (a, b, c, d, e, f) where -}

{- instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g) => ToJSON (a, b, c, d, e, f, g) where -}

{- instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h) => ToJSON (a, b, c, d, e, f, g, h) where -}

{- instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i) => ToJSON (a, b, c, d, e, f, g, h, i) where -}

{- instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j) => ToJSON (a, b, c, d, e, f, g, h, i, j) where -}

{- instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k) => ToJSON (a, b, c, d, e, f, g, h, i, j, k) where -}

{- instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l) => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l) where -}

{- instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m) => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m) where -}

{- instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m, ToJSON n) => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where -}

{- instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m, ToJSON n, ToJSON o) => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where -}

