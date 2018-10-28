{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module Data.Extensible.Instances.FormUrlEncoded where

import Data.Extensible
import Data.Functor.Identity
import Data.Proxy
import GHC.TypeLits
import Web.FormUrlEncoded
import Web.HttpApiData
import qualified Data.HashMap.Strict as HM

instance Forall (KeyValue KnownSymbol (Instance1 FromHttpApiData h)) xs => FromForm (RecordOf h xs) where
  fromForm (Form m) = hgenerateFor (Proxy @ (KeyValue KnownSymbol (Instance1 FromHttpApiData h)))
    $ \k -> case HM.lookup (stringAssocKey k) m of
      Just [v] -> Field <$> parseQueryParam v
      Nothing -> Left $ "missing or multiple field: " <> stringAssocKey k

deriving instance FromHttpApiData a => FromHttpApiData (Identity a)
deriving instance FromHttpApiData (h (AssocValue kv)) => FromHttpApiData (Field h kv)

instance Forall (KeyValue KnownSymbol (Instance1 ToHttpApiData h)) xs => ToForm (RecordOf h xs) where
  toForm = hfoldMapFor (Proxy @ (KeyValue KnownSymbol (Instance1 ToHttpApiData h)))
    (\v -> Form $ HM.singleton (stringAssocKey v) [toQueryParam $ getField v])

deriving instance ToHttpApiData a => ToHttpApiData (Identity a)
deriving instance ToHttpApiData (h (AssocValue kv)) => ToHttpApiData (Field h kv)
