{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TypescriptGenerator where

import Data.Maybe (maybe)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding
import GHC.Generics

import Servant.Foreign
import Servant.JS

import Main

import Text.Groom

-- Dummy type to parameterize instances
data TypeScript

type family (IsTSPrim a) :: Bool where
    IsTSPrim Int = 'True
    IsTSPrim a   = 'False

instance (IsTSPrim a ~ flag, HasForeignType' flag TypeScript Text a) => HasForeignType TypeScript Text a where
    typeFor = typeFor' (Proxy :: Proxy flag)

class HasForeignType' (flag :: Bool) lang ftype a where
    typeFor' :: Proxy flag -> Proxy lang -> Proxy ftype -> Proxy a -> ftype 

instance HasForeignType' 'True TypeScript Text Int where
    typeFor' _ _ _ _ = "number"

instance (Generic a) => HasForeignType' 'False TypeScript Text a where
    typeFor' _ _ _ q = makeType q

class MkTypeScript f where
    makeType :: Proxy (f a) -> Text

instance (Datatype c) => MkTypeScript (D1 c f) where
    makeType x = T.pack (datatypeName x)

writeEndpoint :: Req Text -> Text
writeEndpoint t = queryInterface <>
                  "\texport function " <> functionName <> "(" <> args <> ")\n" <>
                  "\t{\n" <>
                  "\t\t$.ajax({\n" <>
                  "\t\t\t" <> T.intercalate ",\n\t\t\t" ajaxParams <> "\n" <>
                  "\t\t});\n" <>
                  "\t}"
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
          mapSegment (Cap (Arg (PathSegment name) _)) = "encodeURIComponent(" <> name <> ")" 

writeEndpoints :: [Req Text] -> Text
writeEndpoints ts = "namespace Ajax\n" <>
                    "{\n" <>
                    T.intercalate "\n\n" (writeEndpoint <$> ts) <> "\n" <>
                    "}"

g = do
    let output = writeEndpoints $ getEndpoints $ (Proxy :: Proxy InnerApi)
    let jq = jsForAPI (Proxy :: Proxy InnerApi) jquery
    TIO.writeFile "C:/Users/Micah/Source/Austerity/build/Endpoints.ts" (output <> "\n\n" {- <> jq -})
    putStrLn . groom . getEndpoints $ (Proxy :: Proxy InnerApi)