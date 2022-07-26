{-
   Okta API

   Allows customers to easily access the Okta API

   OpenAPI Version: 3.0.1
   Okta API API version: 1.0.0
   Contact: devex-public@okta.com
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Okta.API.Authenticator
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Okta.API.Authenticator where

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


-- ** Authenticator

-- *** activateAuthenticator

-- | @POST \/api\/v1\/authenticators\/{authenticatorId}\/lifecycle\/activate@
-- 
-- Success
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
activateAuthenticator
  :: AuthenticatorId -- ^ "authenticatorId"
  -> OktaRequest ActivateAuthenticator MimeNoContent Authenticator MimeJSON
activateAuthenticator (AuthenticatorId authenticatorId) =
  _mkRequest "POST" ["/api/v1/authenticators/",toPath authenticatorId,"/lifecycle/activate"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ActivateAuthenticator  
-- | @application/json@
instance Produces ActivateAuthenticator MimeJSON


-- *** deactivateAuthenticator

-- | @POST \/api\/v1\/authenticators\/{authenticatorId}\/lifecycle\/deactivate@
-- 
-- Success
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
deactivateAuthenticator
  :: AuthenticatorId -- ^ "authenticatorId"
  -> OktaRequest DeactivateAuthenticator MimeNoContent Authenticator MimeJSON
deactivateAuthenticator (AuthenticatorId authenticatorId) =
  _mkRequest "POST" ["/api/v1/authenticators/",toPath authenticatorId,"/lifecycle/deactivate"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data DeactivateAuthenticator  
-- | @application/json@
instance Produces DeactivateAuthenticator MimeJSON


-- *** getAuthenticator

-- | @GET \/api\/v1\/authenticators\/{authenticatorId}@
-- 
-- Success
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
getAuthenticator
  :: AuthenticatorId -- ^ "authenticatorId"
  -> OktaRequest GetAuthenticator MimeNoContent Authenticator MimeJSON
getAuthenticator (AuthenticatorId authenticatorId) =
  _mkRequest "GET" ["/api/v1/authenticators/",toPath authenticatorId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data GetAuthenticator  
-- | @application/json@
instance Produces GetAuthenticator MimeJSON


-- *** listAuthenticators

-- | @GET \/api\/v1\/authenticators@
-- 
-- Success
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
listAuthenticators
  :: OktaRequest ListAuthenticators MimeNoContent [Authenticator] MimeJSON
listAuthenticators =
  _mkRequest "GET" ["/api/v1/authenticators"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ListAuthenticators  
-- | @application/json@
instance Produces ListAuthenticators MimeJSON


-- *** updateAuthenticator

-- | @PUT \/api\/v1\/authenticators\/{authenticatorId}@
-- 
-- Update Authenticator
-- 
-- Updates an authenticator
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
updateAuthenticator
  :: (Consumes UpdateAuthenticator MimeJSON, MimeRender MimeJSON Authenticator)
  => Authenticator -- ^ "authenticator"
  -> AuthenticatorId -- ^ "authenticatorId"
  -> OktaRequest UpdateAuthenticator MimeJSON Authenticator MimeJSON
updateAuthenticator authenticator (AuthenticatorId authenticatorId) =
  _mkRequest "PUT" ["/api/v1/authenticators/",toPath authenticatorId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` authenticator

data UpdateAuthenticator 
instance HasBodyParam UpdateAuthenticator Authenticator 

-- | @application/json@
instance Consumes UpdateAuthenticator MimeJSON

-- | @application/json@
instance Produces UpdateAuthenticator MimeJSON

