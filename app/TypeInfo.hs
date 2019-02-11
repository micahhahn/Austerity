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

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time
import Data.Typeable
import GHC.Generics

data X = X [Int] [Text]
    deriving (Generic, Typeable)

{- Sadly, I think we will need to rework this approach.  Its going to get hairy around the edges.
   Probably best to create a auto-derivable typeclass to give the user flexibility. -}

makeTypeInfo :: forall t. (Typeable t, Generic t, GDatatypeInfo (Rep t)) => Proxy t -> Map TypeRep DatatypeInfo
makeTypeInfo t = getDatatypeInfo (from (undefined :: t)) (typeRep t) Set.empty

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
    IsPrim ([] a)    = IsPrim a
    IsPrim a         = 'False

class MkTypeInfo (p :: Bool) a where
    mkTypeInfo :: Proxy p -> Proxy a -> Set TypeRep -> Map TypeRep DatatypeInfo

instance MkTypeInfo 'True a where
    mkTypeInfo _ _ _ = Map.empty

instance (Typeable t, Generic t, GDatatypeInfo (Rep t)) => MkTypeInfo 'False t where
    mkTypeInfo _ p ts = getDatatypeInfo (from (undefined :: t)) (typeRep p) ts

class GDatatypeInfo f where
    getDatatypeInfo :: f a -> TypeRep -> Set TypeRep -> Map TypeRep DatatypeInfo

instance (Datatype a, GConstructorInfo c) => GDatatypeInfo (D1 a c) where
    getDatatypeInfo c@(M1 r) t ts = let (cs, m') = getConstructorInfo r (Set.insert t ts)
                                        m = Map.insert t (DatatypeInfo (Text.pack . datatypeName $ c) (Text.pack . moduleName $ c) (Text.pack . packageName $ c) (isNewtype c) cs) Map.empty
                                     in Map.union m m'

class GConstructorInfo f where
    getConstructorInfo :: f a -> Set TypeRep -> ([ConstructorInfo], Map TypeRep DatatypeInfo)

instance (Constructor a, GSelectorInfo c) => GConstructorInfo (C1 a c) where
    getConstructorInfo c@(M1 r) ts = let (t, m) = getSelectorInfo r ts
                                      in ([ConstructorInfo (Text.pack . conName $ c) (conFixity c) (conIsRecord c) t], m)

instance (GConstructorInfo a, GConstructorInfo b) => GConstructorInfo (a :+: b) where
    getConstructorInfo (_ :: (a :+: b) f) ts = let (lc, lm) = getConstructorInfo (undefined :: a f) ts
                                                   (rc, rm) = getConstructorInfo (undefined :: b f) ts
                                               in (lc ++ rc, Map.union lm rm)

class GSelectorInfo f where
    getSelectorInfo :: f a -> Set TypeRep -> ([SelectorInfo], Map TypeRep DatatypeInfo)

instance (Selector a, GFieldInfo c) => GSelectorInfo (S1 a c) where
    getSelectorInfo c@(M1 r) ts = let (t, m) = getFieldInfo r ts
                                   in ([SelectorInfo (Text.pack . selName $ c) (selSourceUnpackedness c) (selSourceStrictness c) (selDecidedStrictness c) t], m)

instance (GSelectorInfo a, GSelectorInfo b) => GSelectorInfo (a :*: b) where
    getSelectorInfo (_ :: (a :*: b) f) ts = let (lt, lm) = getSelectorInfo (undefined :: a f) ts
                                                (rt, rm) = getSelectorInfo (undefined :: b f) ts
                                            in (lt ++ rt, Map.union lm rm)

instance GSelectorInfo U1 where
    getSelectorInfo _ ts = ([], Map.empty)

class GFieldInfo f where
    getFieldInfo :: f a -> Set TypeRep -> (TypeRep, Map TypeRep DatatypeInfo)

instance (IsPrim c ~ p, MkTypeInfo p c, Typeable c) => GFieldInfo (K1 a c) where
    getFieldInfo _ ts = let t = typeRep (Proxy :: Proxy c)
                         in (t, if Set.member t ts then Map.empty else mkTypeInfo (Proxy :: Proxy p) (Proxy :: Proxy c) ts)   

instance GFieldInfo U1 where
    getFieldInfo _ ts = (typeRep (Proxy :: Proxy ()), Map.empty)