{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE DeriveGeneric #-}

module TypeInfo (
    makeTypeInfo,
    DatatypeInfo(..),
    ConstructorInfo(..),
    SelectorInfo(..),
    GDatatypeInfo
) where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Data.Typeable
import GHC.Generics

import Control.Exception

data X = X [Int]
    deriving (Generic, Typeable)

makeTypeInfo :: forall t. (Typeable t, Generic t, GDatatypeInfo (Rep t)) => Proxy t -> Map TypeRep DatatypeInfo
makeTypeInfo t = getDatatypeInfo (from (undefined :: t)) (typeRep (Proxy :: Proxy t))

data DatatypeInfo = DatatypeInfo 
    { _datatypeName :: Text
    , _moduleName :: Text
    , _packageName :: Text
    , _isNewtype :: Bool
    , _constructors :: [ConstructorInfo]
    } deriving (Show)
 
data ConstructorInfo = ConstructorInfo 
    { _conName :: Text 
    , _conFixity :: Fixity
    , _conIsRecord :: Bool
    , _selectors :: [SelectorInfo]
    } deriving (Show)
 
data SelectorInfo = SelectorInfo
    { _selName :: Text
    , _selSourceUnpackedness :: SourceUnpackedness
    , _selSourceStrictness :: SourceStrictness
    , _selDecidedStrictness :: DecidedStrictness
    , _type :: TypeRep
    } deriving (Show)

type family (IsPrim a) :: Bool where
    IsPrim Text      = 'True
    IsPrim Int       = 'True
    IsPrim LocalTime = 'True
    IsPrim Double    = 'True
    IsPrim a         = 'False

x = Map.insert 1 1 Map.empty
y = Map.insert 1 undefined Map.empty

class MkTypeInfo (p :: Bool) a where
    mkTypeInfo :: Proxy p -> Proxy a -> Map TypeRep DatatypeInfo

instance MkTypeInfo 'True a where
    mkTypeInfo _ _ = Map.empty

instance (Typeable t, Generic t, GDatatypeInfo (Rep t)) => MkTypeInfo 'False t where
    mkTypeInfo _ p = makeTypeInfo p

class GDatatypeInfo f where
    getDatatypeInfo :: f a -> TypeRep -> Map TypeRep DatatypeInfo

instance (Datatype a, GConstructorInfo c) => GDatatypeInfo (D1 a c) where
    getDatatypeInfo c@(M1 r) t = let (cs, m') = getConstructorInfo r
                                     m = Map.insert t (DatatypeInfo (Text.pack . datatypeName $ c) (Text.pack . moduleName $ c) (Text.pack . packageName $ c) (isNewtype c) cs) Map.empty
                                  in Map.union m m'

class GConstructorInfo f where
    getConstructorInfo :: f a -> ([ConstructorInfo], Map TypeRep DatatypeInfo)

instance (Constructor a, GSelectorInfo c) => GConstructorInfo (C1 a c) where
    getConstructorInfo c@(M1 r) = let (t, m) = getSelectorInfo r
                                    in ([ConstructorInfo (Text.pack . conName $ c) (conFixity c) (conIsRecord c) t], m)

instance (GConstructorInfo a, GConstructorInfo b) => GConstructorInfo (a :+: b) where
    getConstructorInfo (_ :: (a :+: b) f) = let (lc, lm) = getConstructorInfo (undefined :: a f)
                                                (rc, rm) = getConstructorInfo (undefined :: b f)
                                             in (lc ++ rc, Map.union lm rm)

class GSelectorInfo f where
    getSelectorInfo :: f a -> ([SelectorInfo], Map TypeRep DatatypeInfo)

instance (Selector a, GFieldInfo c) => GSelectorInfo (S1 a c) where
    getSelectorInfo c@(M1 r) = let (t, m) = getFieldInfo r
                                in ([SelectorInfo (Text.pack . selName $ c) (selSourceUnpackedness c) (selSourceStrictness c) (selDecidedStrictness c) t], m)

instance (GSelectorInfo a, GSelectorInfo b) => GSelectorInfo (a :*: b) where
    getSelectorInfo (_ :: (a :*: b) f) = let (lt, lm) = getSelectorInfo (undefined :: a f)
                                             (rt, rm) = getSelectorInfo (undefined :: b f)
                                          in (lt ++ rt, Map.union lm rm)

instance GSelectorInfo U1 where
    getSelectorInfo _ = ([], Map.empty)

class GFieldInfo f where
    getFieldInfo :: f a -> (TypeRep, Map TypeRep DatatypeInfo)

{- List cons is causing infinite recursion -}

instance (IsPrim c ~ p, MkTypeInfo p c, Typeable c) => GFieldInfo (K1 a c) where
    getFieldInfo _ = (typeRep (Proxy :: Proxy c), mkTypeInfo (Proxy :: Proxy p) (Proxy :: Proxy c))

instance GFieldInfo U1 where
    getFieldInfo _ = (typeRep (Proxy :: Proxy ()), Map.empty)