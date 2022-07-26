{-
   Okta API

   Allows customers to easily access the Okta API

   OpenAPI Version: 3.0.1
   Okta API API version: 1.0.0
   Contact: devex-public@okta.com
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Okta.API.Feature
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Okta.API.Feature where

import Okta.Core
import Okta.MimeTypes
import Okta.Model as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Set as Set
import qualified Data.String as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude ((==),(/=),($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

-- * Operations


-- ** Feature

-- *** getFeature

-- | @GET \/api\/v1\/features\/{featureId}@
-- 
-- Success
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
getFeature
  :: FeatureId -- ^ "featureId"
  -> OktaRequest GetFeature MimeNoContent Feature MimeJSON
getFeature (FeatureId featureId) =
  _mkRequest "GET" ["/api/v1/features/",toPath featureId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data GetFeature  
-- | @application/json@
instance Produces GetFeature MimeJSON


-- *** listFeatureDependencies

-- | @GET \/api\/v1\/features\/{featureId}\/dependencies@
-- 
-- Success
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
listFeatureDependencies
  :: FeatureId -- ^ "featureId"
  -> OktaRequest ListFeatureDependencies MimeNoContent [Feature] MimeJSON
listFeatureDependencies (FeatureId featureId) =
  _mkRequest "GET" ["/api/v1/features/",toPath featureId,"/dependencies"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ListFeatureDependencies  
-- | @application/json@
instance Produces ListFeatureDependencies MimeJSON


-- *** listFeatureDependents

-- | @GET \/api\/v1\/features\/{featureId}\/dependents@
-- 
-- Success
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
listFeatureDependents
  :: FeatureId -- ^ "featureId"
  -> OktaRequest ListFeatureDependents MimeNoContent [Feature] MimeJSON
listFeatureDependents (FeatureId featureId) =
  _mkRequest "GET" ["/api/v1/features/",toPath featureId,"/dependents"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ListFeatureDependents  
-- | @application/json@
instance Produces ListFeatureDependents MimeJSON


-- *** listFeatures

-- | @GET \/api\/v1\/features@
-- 
-- Success
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
listFeatures
  :: OktaRequest ListFeatures MimeNoContent [Feature] MimeJSON
listFeatures =
  _mkRequest "GET" ["/api/v1/features"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ListFeatures  
-- | @application/json@
instance Produces ListFeatures MimeJSON


-- *** updateFeatureLifecycle

-- | @POST \/api\/v1\/features\/{featureId}\/{lifecycle}@
-- 
-- Success
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
updateFeatureLifecycle
  :: FeatureId -- ^ "featureId"
  -> Lifecycle -- ^ "lifecycle"
  -> OktaRequest UpdateFeatureLifecycle MimeNoContent Feature MimeJSON
updateFeatureLifecycle (FeatureId featureId) (Lifecycle lifecycle) =
  _mkRequest "POST" ["/api/v1/features/",toPath featureId,"/",toPath lifecycle]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data UpdateFeatureLifecycle  
instance HasOptionalParam UpdateFeatureLifecycle Mode where
  applyOptionalParam req (Mode xs) =
    req `addQuery` toQuery ("mode", Just xs)
-- | @application/json@
instance Produces UpdateFeatureLifecycle MimeJSON
