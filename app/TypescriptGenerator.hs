{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE DeriveGeneric #-}

module TypescriptGenerator where

import Data.Maybe (maybe)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding
import GHC.Generics

import Servant.Foreign

import Main

import Data.Typeable

import Debug.Trace
import Text.Groom

import TypeInfo

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Time

-- Dummy type to parameterize instances
data TypeScript

getTSName :: TypeRep -> Text
getTSName t = name <> argText
    where name = if typeRepTyCon t == (typeRepTyCon . typeRep $ (Proxy :: Proxy [()]))
                 then "Array"
                 else (T.pack . tyConName . typeRepTyCon $ t)

          args = getTSName <$> typeRepArgs t
          argText = if null args then "" else "<" <> T.intercalate ", " args <> ">"

data TypeContext a = TypeContext a (Map TypeRep DatatypeInfo)
    deriving (Functor)

instance Applicative TypeContext where
    pure a = TypeContext a (Map.empty)
    (<*>) (TypeContext f m) (TypeContext a m') = TypeContext (f a) (Map.union m m')

instance Monad TypeContext where
    return a = TypeContext a (Map.empty)

    (>>=) (TypeContext a m) f = let (TypeContext a' m') = f a
                                 in TypeDesc a' (Map.union m m') 

type family (IsTSPrim a) :: Bool where
    IsTSPrim Int       = 'True
    IsTSPrim Text      = 'True
    IsTSPrim LocalTime = 'True
    IsTSPrim Double    = 'True
    IsTSPrim a         = 'False

instance (IsTSPrim a ~ flag, HasForeignType' flag TypeScript (TypeDesc Text) a) => HasForeignType TypeScript (TypeDesc Text) a where
    typeFor = typeFor' (Proxy :: Proxy flag)

class HasForeignType' (flag :: Bool) lang ftype a where
    typeFor' :: Proxy flag -> Proxy lang -> Proxy ftype -> Proxy a -> ftype 

instance HasForeignType' 'True TypeScript (TypeDesc Text) Int where
    typeFor' _ _ _ _ = return "number"

instance HasForeignType' 'True TypeScript (TypeDesc Text) Double where
    typeFor' _ _ _ _ = return "number"

instance HasForeignType' 'True TypeScript (TypeDesc Text) Text where
    typeFor' _ _ _ _ = return "string"

instance HasForeignType' 'True TypeScript (TypeDesc Text) LocalTime where
    typeFor' _ _ _ _ = return "string"

instance (Generic a, Typeable a, GDatatypeInfo (Rep a)) => HasForeignType' 'False TypeScript (TypeDesc Text) a where
    typeFor' _ _ _ q = TypeDesc (getTSName $ typeRep (Proxy :: Proxy a)) (makeTypeInfo (Proxy :: Proxy a))

writeEndpoint :: Req (TypeDesc Text) -> TypeDesc Text
writeEndpoint t = do
    let functionName = mconcat . map T.toTitle . unFunctionName . _reqFuncName $ t
    successType <- maybe (return "void") id (_reqReturnType t)
    {-let url = "\"/\" + " <> T.intercalate "+ \"/\" + " (mapSegment . unSegment <$> (_path . _reqUrl $ t))
    captures <- sequence [type' >>= (\t -> return $ name <> ": " <> t) | Cap (Arg (PathSegment name) type') <- (unSegment <$> (_path . _reqUrl $ t))]
    successType <- maybe (return "void") id (_reqReturnType t)
    let requiredArgs = ["onSuccess: (s: " <> successType <> ") => void", "onError: (s) => void"]
    let args = T.intercalate ", " (captures ++ requiredArgs)-}
    return $ "\texport function " <> functionName <> "(" <> ")\n" <>
             "\t{\n" <> successType <>
             "\t}"

    where mapSegment :: SegmentType (TypeDesc Text) -> Text
          mapSegment (Static (PathSegment s)) = "\"" <> s <> "\""
          mapSegment (Cap (Arg (PathSegment name) _)) = "encodeURIComponent(" <> name <> ")"

    {-(queryInterface <>
                  "\texport function " <> functionName <> "(" <> args <> ")\n" <>
                  "\t{\n" <>
                  "\t\t$.ajax({\n" <>
                  "\t\t\t" <> T.intercalate ",\n\t\t\t" ajaxParams <> "\n" <>
                  "\t\t});\n" <>
                  "\t}", )
    where functionName = mconcat . map T.toTitle . unFunctionName . _reqFuncName $ t
          url = "url: \"/\" + " <> (T.intercalate "+ \"/\" + " $ map (mapSegment . unSegment) (_path . _reqUrl $ t))
          captures = [ name <> ": " <> type' | Cap (Arg (PathSegment name) type') <- (unSegment <$> (_path . _reqUrl $ t))]
          query = (\(Arg (PathSegment name) type') -> name <> ": " <> type' <> ";") . _queryArgName <$> (_queryStr . _reqUrl $ t)
          requiredArgs = ["onSuccess: (s: " <> successType <> ") => void", "onError: (s) => void"]
          args = T.intercalate ", " (captures ++ queryParam ++ requiredArgs)
          successType = maybe "void" id (_reqReturnType t)

          (queryParam, queryInterface) = if null . _queryStr . _reqUrl $ t
                                         then ([], "")
                                         else (["$query: I" <> functionName <> "QueryParams"]
                                              , "\texport interface I" <> functionName <> "QueryParams\n" <>
                                                "\t{\n" <>
                                                "\t\t" <> T.intercalate "\n\t\t" query <> "\n" <>
                                                "\t}\n\n")
                                         
          ajaxParams = [url, "success: onSuccess", "error: onError", "type: '" <> decodeUtf8 (_reqMethod t) <> "'"]

          mapSegment :: SegmentType Text -> Text
          mapSegment (Static (PathSegment s)) = "\"" <> s <> "\""
          mapSegment (Cap (Arg (PathSegment name) _)) = "encodeURIComponent(" <> name <> ")"  -}

writeEndpoints :: [Req (TypeDesc Text)] -> Text
writeEndpoints ts = let (TypeDesc ts' m) = sequence (writeEndpoint <$> ts)
                     in "namespace Ajax\n" <>
                        "{\n" <>
                        T.intercalate "\n\n" ts' <> "\n" <>
                        "}"

getEndpoints = listFromAPI (Proxy :: Proxy TypeScript) (Proxy :: Proxy (TypeDesc Text))
endpoints = getEndpoints (Proxy :: Proxy InnerApi)

g = do
    let output = writeEndpoints $ getEndpoints $ (Proxy :: Proxy InnerApi)
    {- let jq = jsForAPI (Proxy :: Proxy InnerApi) jquery -}
    TIO.writeFile "./build/Endpoints.ts" (output <> "\n\n" {- <> jq -})