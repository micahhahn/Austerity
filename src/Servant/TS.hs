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
    TsType(..),
    TsContext(..),
    TsTypeable(..),
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

instance (TsTypeable a) => HasForeignType TypeScript (TsContext TsType) a where
    typeFor _ _ p = tsTypeRep p

tsForAPI :: (HasForeign TypeScript (TsContext TsType) api, GenerateList (TsContext TsType) (Foreign (TsContext TsType) api)) => Proxy api -> Text
tsForAPI api = writeEndpoints $ listFromAPI (Proxy :: Proxy TypeScript) (Proxy :: Proxy (TsContext TsType)) api

{- Using 'data' in jquery options potentially incorrect? -}

tsTypeName :: TsType -> Text 
tsTypeName TsVoid = "void"
tsTypeName TsNever = "never"
tsTypeName TsBoolean = "boolean"
tsTypeName TsNumber = "number"
tsTypeName TsString = "string"
tsTypeName (TsNullable t) = tsTypeName t <> "?"
tsTypeName (TsRef t) = sanitizeTSName . Text.pack . show $ typeRepTyCon t 
tsTypeName (TsArray t) = "Array<" <> tsTypeName t <> ">"

writeEndpoint :: Req (TsContext TsType) -> TsContext Text
writeEndpoint t = do
    let functionName = mconcat . map Text.toTitle . unFunctionName . _reqFuncName $ t
    let method = TE.decodeUtf8 $ _reqMethod t
    successType <- maybe (return TsVoid) id (_reqReturnType t)
    captures <- sequence [type' >>= (\t -> return $ name <> ": " <> tsTypeName t) | Cap (Arg (PathSegment name) type') <- (unSegment <$> (_path . _reqUrl $ t))]
    (bodyArg, bodyJQueryArg) <- maybe (return ([], [])) (fmap (\t -> (["$body: " <> tsTypeName t], [("data", "$body")]))) $ _reqBody t
    
    q <- sequence $ (\a -> do
                        t <- _argType . _queryArgName $ a
                        return (unPathSegment . _argName . _queryArgName $ a, t)
                    ) <$> (_queryStr . _reqUrl $ t)
    
    let queryArgs = if null q then [] else [("$query: {" <> Text.intercalate ", " ((\(l, r) -> l <> ": " <> tsTypeName r) <$> q) <> "}")]

    let queryPrepare = if null q then ""
                                 else "\t\tlet $queryArgs : string[] = [];\n" <>
                                      Text.intercalate "\n" ((\n -> "\t\tif ($query." <> n <> " !== undefined)\n" <>
                                                                    "\t\t\t$queryArgs.push(encodeURIComponent(String($query." <> n <> ")));\n") . fst <$> q) <> "\n" <>
                                      "\t\tlet $queryString = $queryArgs.length == 0 ? \"\" : \"?\" + $queryArgs.join(\"&\");\n\n"

    let url = "\"/\" + " <> (Text.intercalate "+ \"/\" + " (mapSegment . unSegment <$> (_path . _reqUrl $ t))) <>
              if null q then "" else " + $queryString"

    let args = captures ++ queryArgs ++ bodyArg ++ ["onSuccess: (result: " <> tsTypeName successType <> ") => void", "onError: () => void"]
    let jqueryArgs = [("url", url), ("success", "onSuccess"), ("error", "onError"), ("method", "\"" <> method <> "\"")] ++ bodyJQueryArg
    return $ "\texport function " <> functionName <> "(" <> Text.intercalate ", " args <> "): void\n" <>
             "\t{\n" <>
             queryPrepare <>
             "\t\t$.ajax({\n\t\t\t" <> Text.intercalate ",\n\t\t\t" ((\(l, r) -> l <> ": " <> r) <$> jqueryArgs) <> "\n\t\t});\n" <>
             "\t}"

    where mapSegment :: SegmentType (TsContext TsType) -> Text
          mapSegment (Static (PathSegment s)) = "\"" <> s <> "\""
          mapSegment (Cap (Arg (PathSegment name) _)) = "encodeURIComponent(String(" <> name <> "))"

          writeTsType :: TsType -> Text
          writeTsType TsNever = "never"
          writeTsType TsBoolean = "boolean"
          writeTsType TsNumber = "number"
          writeTsType TsString = "string"
          writeTsType (TsUnion ts) = Text.intercalate " | " (writeTsType <$> ts)
          writeTsType (TsNullable t) = (writeTsType t) <> " | null"

writeEndpoints :: [Req (TsContext TsType)] -> Text
writeEndpoints ts = let (TsContext ts' m) = sequence (writeEndpoint <$> ts)
                     in "namespace Ajax\n" <>
                        "{\n" <>
                        {- Text.intercalate "\n\n" (Map.elems m) <> "\n" <> -}
                        Text.intercalate "\n\n" ts' <> "\n" <>
                        "}"

data TsType = TsVoid
            | TsNever
            | TsBoolean
            | TsNumber
            | TsString
            | TsUnion [TsType]
            | TsNullable TsType
            | TsArray TsType
            | TsObject Text [(Text, TsType)]
            | TsTuple Text [TsType]
            | TsRef TypeRep
    deriving (Show)

data TsContext a = TsContext a (Map TypeRep TsType)
    deriving (Show, Functor)

instance Applicative TsContext where
    pure a = TsContext a (Map.empty)
    (<*>) (TsContext f m) (TsContext a m') = TsContext (f a) (Map.union m m')

instance Monad TsContext where
    return = pure
    (>>=) (TsContext a m) f = let (TsContext a' m') = f a
                               in TsContext a' (Map.union m m')

sanitizeTSName :: Text -> Text
sanitizeTSName = Text.replace "'" ""

class TsTypeable a where
    tsTypeRep :: Proxy a -> TsContext TsType
    default tsTypeRep :: (Generic a, Typeable a, TsDatatype (Rep a)) => Proxy a -> TsContext TsType
    tsTypeRep p = tsDatatype (from (undefined :: a)) (typeRep p)

class TsDatatype a where
    tsDatatype :: a p -> TypeRep -> TsContext TsType

instance (Datatype a, TsConstructor c) => TsDatatype (D1 a c) where
    tsDatatype c@(M1 r) t = do
        cons <- tsConstructor r
        let tsType = if length cons == 1 then head cons
                                         else TsUnion cons
        TsContext (TsRef t) (Map.insert t tsType Map.empty)

class TsConstructor a where
    tsConstructor :: a p -> TsContext [TsType]

instance (TsConstructor a, TsConstructor b) => TsConstructor (a :+: b) where
    tsConstructor (_ :: (a :+: b) f) = do
        l <- (tsConstructor (undefined :: a f))
        r <- (tsConstructor (undefined :: b f))
        return $ l ++ r
                                         
instance (Constructor a, TsSelector c) => TsConstructor (C1 a c) where
    tsConstructor c@(M1 r) = do
        sels <- tsSelector r
        let n = sanitizeTSName . Text.pack . conName $ c
        let tsType = if conIsRecord c then TsObject n sels else TsTuple n (snd <$> sels)
        return [tsType]

class TsSelector a where
    tsSelector :: a p -> TsContext [(Text, TsType)]

instance (TsSelector a, TsSelector b) => TsSelector (a :*: b) where
    tsSelector (_ :: (a :*: b) f) = do
        l <- tsSelector (undefined :: a f)
        r <- tsSelector (undefined :: b f)
        return $ l ++ r

instance (Selector a, TsTypeable c) => TsSelector (S1 a (K1 b c)) where
    tsSelector q@(M1 (K1 t)) = do
        tsType <- tsTypeRep (Proxy :: Proxy c)
        return [(Text.pack . selName $ q, tsType)]

instance TsSelector U1 where
    tsSelector _ = return []

instance TsTypeable Bool where
    tsTypeRep _ = return TsBoolean

instance TsTypeable Ordering where
    tsTypeRep _ = return TsString

instance TsTypeable () where
    tsTypeRep _ = return $ TsArray TsNever

instance TsTypeable Char where
    tsTypeRep _ = return TsString

instance TsTypeable Double where
    tsTypeRep _ = return TsNumber

{-
instance TsType Number where
    tsType _ = "number"
-}

instance TsTypeable Float where
    tsTypeRep _ = return TsNumber

instance (TsTypeable a, Integral a) => TsTypeable (Ratio a) where
    tsTypeRep _ = return TsNumber
    
instance (HasResolution a) => TsTypeable (Fixed a) where
    tsTypeRep _ = return TsNumber

instance TsTypeable Int where
    tsTypeRep _ = return TsNumber

instance TsTypeable Integer where 
    tsTypeRep _ = return TsNumber

instance TsTypeable Natural where
    tsTypeRep _ = return TsNumber

instance TsTypeable Int8 where
    tsTypeRep _ = return TsNumber

instance TsTypeable Int16 where
    tsTypeRep _ = return TsNumber

instance TsTypeable Int32 where
    tsTypeRep _ = return TsNumber

instance TsTypeable Int64 where
    tsTypeRep _ = return TsNumber

instance TsTypeable Word where
    tsTypeRep _ = return TsNumber

instance TsTypeable Word8 where
    tsTypeRep _ = return TsNumber

instance TsTypeable Word16 where
    tsTypeRep _ = return TsNumber

instance TsTypeable Word32 where
    tsTypeRep _ = return TsNumber

instance TsTypeable Word64 where
    tsTypeRep _ = return TsNumber

instance TsTypeable CTime where
    tsTypeRep _ = return TsNumber

instance TsTypeable Text where
    tsTypeRep _ = return TsString

instance TsTypeable LT.Text where
    tsTypeRep _ = return TsString

instance TsTypeable Version where
    tsTypeRep _ = return TsString

instance (TsTypeable a) => TsTypeable (Maybe a) where
    tsTypeRep _ = TsNullable <$> (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeable a) => TsTypeable [a] where
    tsTypeRep _ = TsArray <$> (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeable a) => TsTypeable (NonEmpty a) where
    tsTypeRep _ = TsArray <$> (tsTypeRep (Proxy :: Proxy a))

{-
instance TsType Scientific where
    tsType _ = "number"
-}

{-
instance (TsType a) => TsType (DList a) where
    tsType _ = "Array<" <> tsType (Proxy :: Proxy a) <> ">"
-}

instance (TsTypeable a) => TsTypeable (Identity a) where
    tsTypeRep _ = tsTypeRep (Proxy :: Proxy a)

{-
instance (TsType a, TsType b) => TsType (Compose f a b) where
    tsType _ = 
-}

{-
instance (TsType a, TsType b) => TsType (Product f a b) where
    tsType _ = 
-}

instance (TsTypeable a) => TsTypeable (Seq a) where
    tsTypeRep _ = TsArray <$> (tsTypeRep (Proxy :: Proxy a))

instance (TsTypeable a) => TsTypeable (Set a) where
    tsTypeRep _ = TsArray <$> (tsTypeRep (Proxy :: Proxy a))

instance TsTypeable IntSet where
    tsTypeRep _ = return $ TsArray TsNumber

{- instance TsType IntMap where -}

{- instance TsType Tree where -}

{- instance TsType UUID -}

instance (TsTypeable a) => TsTypeable (V.Vector a) where
    tsTypeRep _ = TsArray <$> (tsTypeRep (Proxy :: Proxy a))

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

instance TsTypeable LocalTime where
    tsTypeRep _ = return TsString

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

