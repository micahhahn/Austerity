{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Servant.TS (
    tsForAPI,
    TSType(..),
    TypeContext(..),
    TypeConstructor(..),
    TypeScript(..)
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
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Data.Time
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

import Servant.Foreign

-- Dummy type to parameterize instances
data TypeScript

instance (TSType a) => HasForeignType TypeScript (TypeContext Text) a where
    typeFor _ _ p = tsType p

tsForAPI :: (HasForeign TypeScript (TypeContext Text) api, GenerateList (TypeContext Text) (Foreign (TypeContext Text) api)) => Proxy api -> Text
tsForAPI api = writeEndpoints $ listFromAPI (Proxy :: Proxy TypeScript) (Proxy :: Proxy (TypeContext Text)) api

{- Using 'data' in jquery options potentially incorrect? -}

writeEndpoint :: Req (TypeContext Text) -> TypeContext Text
writeEndpoint t = do
    let functionName = mconcat . map Text.toTitle . unFunctionName . _reqFuncName $ t
    let method = TE.decodeUtf8 $ _reqMethod t
    successType <- maybe (return "void") id (_reqReturnType t)
    captures <- sequence [type' >>= (\t -> return $ name <> ": " <> t) | Cap (Arg (PathSegment name) type') <- (unSegment <$> (_path . _reqUrl $ t))]
    (bodyArg, bodyJQueryArg) <- maybe (return ([], [])) (fmap (\t -> (["$body: " <> t], [("data", "$body")]))) $ _reqBody t
    
    q <- sequence $ (\a -> do
                        t <- _argType . _queryArgName $ a
                        return (unPathSegment . _argName . _queryArgName $ a, t)
                    ) <$> (_queryStr . _reqUrl $ t)
    
    let queryArgs = if null q then [] else [("$query: {" <> Text.intercalate ", " ((\(l, r) -> l <> ": " <> r) <$> q) <> "}")]

    let queryPrepare = if null q then ""
                                 else "\t\tlet $queryArgs : string[] = [];\n" <>
                                      Text.intercalate "\n" ((\n -> "\t\tif ($query." <> n <> " !== undefined)\n" <>
                                                                    "\t\t\t$queryArgs.push(encodeURIComponent(String($query." <> n <> ")));\n") . fst <$> q) <> "\n" <>
                                      "\t\tlet $queryString = $queryArgs.length == 0 ? \"\" : \"?\" + $queryArgs.join(\"&\");\n\n"

    let url = "\"/\" + " <> (Text.intercalate "+ \"/\" + " (mapSegment . unSegment <$> (_path . _reqUrl $ t))) <>
              if null q then "" else " + $queryString"

    let args = captures ++ queryArgs ++ bodyArg ++ ["onSuccess: (result: " <> successType <> ") => void", "onError: () => void"]
    let jqueryArgs = [("url", url), ("success", "onSuccess"), ("error", "onError"), ("method", "\"" <> method <> "\"")] ++ bodyJQueryArg
    return $ "\texport function " <> functionName <> "(" <> Text.intercalate ", " args <> "): void\n" <>
             "\t{\n" <>
             queryPrepare <>
             "\t\t$.ajax({\n\t\t\t" <> Text.intercalate ",\n\t\t\t" ((\(l, r) -> l <> ": " <> r) <$> jqueryArgs) <> "\n\t\t});\n" <>
             "\t}"

    where mapSegment :: SegmentType (TypeContext Text) -> Text
          mapSegment (Static (PathSegment s)) = "\"" <> s <> "\""
          mapSegment (Cap (Arg (PathSegment name) _)) = "encodeURIComponent(String(" <> name <> "))"

writeEndpoints :: [Req (TypeContext Text)] -> Text
writeEndpoints ts = let (TypeContext ts' m) = sequence (writeEndpoint <$> ts)
                     in "namespace Ajax\n" <>
                        "{\n" <>
                        Text.intercalate "\n\n" (Map.elems m) <> "\n" <>
                        Text.intercalate "\n\n" ts' <> "\n" <>
                        "}"

data TsType = TsBoolean
            | TsNumber
            | TsString
            | TsUnion [(Text, TsType)]
            | TsNullable TsType
            | TsObject [(Text, TsType)]
            | TsTuple [TsType]
            | TsArray TsType
    deriving (Show)

data TypeConstructor = TypeConstructor
    { datatype :: TypeRep
    , constructor :: Text
    } deriving (Eq, Ord, Show)

data TypeContext a = TypeContext a (Map TypeConstructor Text)
    deriving (Show, Functor)

instance Applicative TypeContext where
    pure a = TypeContext a (Map.empty)
    (<*>) (TypeContext f m) (TypeContext a m') = TypeContext (f a) (Map.union m m')

instance Monad TypeContext where
    return = pure

    (>>=) (TypeContext a m) f = let (TypeContext a' m') = f a
                                 in TypeContext a' (Map.union m m') 

sanitizeTSName :: Text -> Text
sanitizeTSName = Text.replace "'" ""

class TSType a where
    tsType :: Proxy a -> TypeContext Text
    default tsType :: (Generic a, Typeable a, TSDatatype (Rep a)) => Proxy a -> TypeContext Text
    tsType _ = tsDatatype (from (undefined :: a)) (typeRep (Proxy :: Proxy a))

class TSDatatype a where
    tsDatatype :: a p -> TypeRep -> TypeContext Text

instance (Datatype a, TSConstructor c) => TSDatatype (D1 a c) where
    tsDatatype c@(M1 r) t = tsConstructor r t False

class TSConstructor a where
    tsConstructor :: a p -> TypeRep -> Bool -> TypeContext Text

instance (TSConstructor a, TSConstructor b) => TSConstructor (a :+: b) where
    tsConstructor (_ :: (a :+: b) f) t _ = do
        l <- tsConstructor (undefined :: a f) t True
        r <- tsConstructor (undefined :: b f) t True
        return $ l <> " | " <> r
                                         
instance (Constructor a, TSSelector c) => TSConstructor (C1 a c) where
    tsConstructor c@(M1 r) t isTagged = do
        selectors <- tsSelector r
        let con = TypeConstructor t (sanitizeTSName . Text.pack . conName $ c)
        let interface = ("\tinterface " <> (sanitizeTSName . Text.pack . conName $ c) <> "\n" <>
                         "\t{\n" <>
                         (if isTagged then "\t\ttag: \"" <> (sanitizeTSName . Text.pack . conName $ c) <> "\";\n" else "") <>
                         (if null selectors then "" else 
                            if conIsRecord c then mconcat $ (\(l, r) -> "\t\t" <> l <> ": " <> r <> ";\n") <$> selectors 
                                             else "\t\tcontents: [" <> Text.intercalate ", " (snd <$> selectors) <> "];" ) <>
                         "\t}\n")
        TypeContext (sanitizeTSName . Text.pack . conName $ c) (Map.insert con interface Map.empty)

class TSSelector a where
    tsSelector :: a p -> TypeContext [(Text, Text)]

instance (TSSelector a, TSSelector b) => TSSelector (a :*: b) where
    tsSelector (_ :: (a :*: b) f) = do
        l <- tsSelector (undefined :: a f)
        r <- tsSelector (undefined :: b f)
        return (l ++ r)

instance (Selector a, TSType c) => TSSelector (S1 a (K1 b c)) where
    tsSelector c@(M1 (K1 t)) = do
        field <- tsType (Proxy :: Proxy c)
        return [(Text.pack . selName $ c, field)]

{- instance (TSType c) => TSSelector (K1 a c) where
    tsSelector _ = undefined -}

instance TSSelector U1 where
    tsSelector _ = return []

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

instance (TSType a) => TSType (Maybe a) where
    tsType _ = do
        ts <- tsType (Proxy :: Proxy a)
        return $ ts <> " | null"

instance (TSType a) => TSType [a] where
    tsType _ = do
        ts <- tsType (Proxy :: Proxy a)
        return $ "Array<" <> ts <> ">"

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

instance TSType LocalTime where
    tsType _ = return "string"

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

