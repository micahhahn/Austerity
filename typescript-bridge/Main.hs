module Main where

import Data.Proxy
import qualified Data.Text.IO as TIO

import AusterityApi
import Servant.TS

main :: IO ()
main = do
    let t = tsForAPI (Proxy :: Proxy AusterityApi) (TsGenOptions TsSingleQuotes TsIndentTab)
    TIO.writeFile "C:/Users/Micah/Source/Austerity/build/Endpoints.ts" t