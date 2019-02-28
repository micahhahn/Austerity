module Main where

import Data.Proxy
import qualified Data.Text.IO as TIO
import System.Environment

import AusterityApi
import Servant.TS

main :: IO ()
main = do
    args <- getArgs
    if length args < 1 then putStrLn "Expected path name as argument 1."
                       else do let t = tsForAPI (Proxy :: Proxy AusterityApi) (TsGenOptions TsSingleQuotes TsIndentTab)
                               TIO.writeFile (head args) t