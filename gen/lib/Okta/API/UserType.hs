{-
   Okta API

   Allows customers to easily access the Okta API

   OpenAPI Version: 3.0.1
   Okta API API version: 1.0.0
   Contact: devex-public@okta.com
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Okta.API.UserType
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Okta.API.UserType where

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


-- ** UserType

-- *** createUserType

-- | @POST \/api\/v1\/meta\/types\/user@
-- 
-- Creates a new User Type. A default User Type is automatically created along with your org, and you may add another 9 User Types for a maximum of 10.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
createUserType
  :: (Consumes CreateUserType MimeJSON, MimeRender MimeJSON UserType)
  => UserType -- ^ "userType"
  -> OktaRequest CreateUserType MimeJSON UserType MimeJSON
createUserType userType =
  _mkRequest "POST" ["/api/v1/meta/types/user"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` userType

data CreateUserType 
instance HasBodyParam CreateUserType UserType 

-- | @application/json@
instance Consumes CreateUserType MimeJSON

-- | @application/json@
instance Produces CreateUserType MimeJSON


-- *** deleteUserType

-- | @DELETE \/api\/v1\/meta\/types\/user\/{typeId}@
-- 
-- Deletes a User Type permanently. This operation is not permitted for the default type, nor for any User Type that has existing users
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
deleteUserType
  :: TypeId -- ^ "typeId"
  -> OktaRequest DeleteUserType MimeNoContent NoContent MimeNoContent
deleteUserType (TypeId typeId) =
  _mkRequest "DELETE" ["/api/v1/meta/types/user/",toPath typeId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data DeleteUserType  
instance Produces DeleteUserType MimeNoContent


-- *** getUserType

-- | @GET \/api\/v1\/meta\/types\/user\/{typeId}@
-- 
-- Fetches a User Type by ID. The special identifier `default` may be used to fetch the default User Type.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
getUserType
  :: TypeId -- ^ "typeId"
  -> OktaRequest GetUserType MimeNoContent UserType MimeJSON
getUserType (TypeId typeId) =
  _mkRequest "GET" ["/api/v1/meta/types/user/",toPath typeId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data GetUserType  
-- | @application/json@
instance Produces GetUserType MimeJSON


-- *** listUserTypes

-- | @GET \/api\/v1\/meta\/types\/user@
-- 
-- Fetches all User Types in your org
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
listUserTypes
  :: OktaRequest ListUserTypes MimeNoContent [UserType] MimeJSON
listUserTypes =
  _mkRequest "GET" ["/api/v1/meta/types/user"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ListUserTypes  
-- | @application/json@
instance Produces ListUserTypes MimeJSON


-- *** replaceUserType

-- | @PUT \/api\/v1\/meta\/types\/user\/{typeId}@
-- 
-- Replace an existing User Type
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
replaceUserType
  :: (Consumes ReplaceUserType MimeJSON, MimeRender MimeJSON UserType)
  => UserType -- ^ "userType"
  -> TypeId -- ^ "typeId"
  -> OktaRequest ReplaceUserType MimeJSON UserType MimeJSON
replaceUserType userType (TypeId typeId) =
  _mkRequest "PUT" ["/api/v1/meta/types/user/",toPath typeId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` userType

data ReplaceUserType 
instance HasBodyParam ReplaceUserType UserType 

-- | @application/json@
instance Consumes ReplaceUserType MimeJSON

-- | @application/json@
instance Produces ReplaceUserType MimeJSON


-- *** updateUserType

-- | @POST \/api\/v1\/meta\/types\/user\/{typeId}@
-- 
-- Updates an existing User Type
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
updateUserType
  :: (Consumes UpdateUserType MimeJSON, MimeRender MimeJSON UserType)
  => UserType -- ^ "userType"
  -> TypeId -- ^ "typeId"
  -> OktaRequest UpdateUserType MimeJSON UserType MimeJSON
updateUserType userType (TypeId typeId) =
  _mkRequest "POST" ["/api/v1/meta/types/user/",toPath typeId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` userType

data UpdateUserType 
instance HasBodyParam UpdateUserType UserType 

-- | @application/json@
instance Consumes UpdateUserType MimeJSON

-- | @application/json@
instance Produces UpdateUserType MimeJSON

