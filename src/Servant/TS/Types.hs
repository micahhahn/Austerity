{-# LANGUAGE DeriveFunctor #-}

module Servant.TS.Types (
    TypeContext(..)
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable
import GHC.Generics

data TypeContext a = TypeContext a (Map TypeRep Text)
    deriving (Show, Functor)

instance Applicative TypeContext where
    pure a = TypeContext a (Map.empty)
    (<*>) (TypeContext f m) (TypeContext a m') = TypeContext (f a) (Map.union m m')

instance Monad TypeContext where
    return = pure

    (>>=) (TypeContext a m) f = let (TypeContext a' m') = f a
                                 in TypeContext a' (Map.union m m') 

