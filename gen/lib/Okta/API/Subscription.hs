{-
   Okta API

   Allows customers to easily access the Okta API

   OpenAPI Version: 3.0.1
   Okta API API version: 1.0.0
   Contact: devex-public@okta.com
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Okta.API.Subscription
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Okta.API.Subscription where

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


-- ** Subscription

-- *** getRoleSubscriptionByNotificationType

-- | @GET \/api\/v1\/roles\/{roleTypeOrRoleId}\/subscriptions\/{notificationType}@
-- 
-- Get subscriptions of a Custom Role with a specific notification type
-- 
-- When roleType Get subscriptions of a Role with a specific notification type. Else when roleId Get subscription of a Custom Role with a specific notification type.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
getRoleSubscriptionByNotificationType
  :: RoleTypeOrRoleId -- ^ "roleTypeOrRoleId"
  -> NotificationTypeText -- ^ "notificationType"
  -> OktaRequest GetRoleSubscriptionByNotificationType MimeNoContent Subscription MimeJSON
getRoleSubscriptionByNotificationType (RoleTypeOrRoleId roleTypeOrRoleId) (NotificationTypeText notificationType) =
  _mkRequest "GET" ["/api/v1/roles/",toPath roleTypeOrRoleId,"/subscriptions/",toPath notificationType]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data GetRoleSubscriptionByNotificationType  
-- | @application/json@
instance Produces GetRoleSubscriptionByNotificationType MimeJSON


-- *** listRoleSubscriptions

-- | @GET \/api\/v1\/roles\/{roleTypeOrRoleId}\/subscriptions@
-- 
-- List all subscriptions of a Custom Role
-- 
-- When roleType List all subscriptions of a Role. Else when roleId List subscriptions of a Custom Role
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
listRoleSubscriptions
  :: RoleTypeOrRoleId -- ^ "roleTypeOrRoleId"
  -> OktaRequest ListRoleSubscriptions MimeNoContent [Subscription] MimeJSON
listRoleSubscriptions (RoleTypeOrRoleId roleTypeOrRoleId) =
  _mkRequest "GET" ["/api/v1/roles/",toPath roleTypeOrRoleId,"/subscriptions"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ListRoleSubscriptions  
-- | @application/json@
instance Produces ListRoleSubscriptions MimeJSON


-- *** subscribeRoleSubscriptionByNotificationType

-- | @POST \/api\/v1\/roles\/{roleTypeOrRoleId}\/subscriptions\/{notificationType}\/subscribe@
-- 
-- Subscribe a Custom Role to a specific notification type
-- 
-- When roleType Subscribes a Role to a specific notification type. When you change the subscription status of a Role, it overrides the subscription of any individual user of that Role. Else when roleId Subscribes a Custom Role to a specific notification type. When you change the subscription status of a Custom Role, it overrides the subscription of any individual user of that Custom Role.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
subscribeRoleSubscriptionByNotificationType
  :: RoleTypeOrRoleId -- ^ "roleTypeOrRoleId"
  -> NotificationTypeText -- ^ "notificationType"
  -> OktaRequest SubscribeRoleSubscriptionByNotificationType MimeNoContent NoContent MimeNoContent
subscribeRoleSubscriptionByNotificationType (RoleTypeOrRoleId roleTypeOrRoleId) (NotificationTypeText notificationType) =
  _mkRequest "POST" ["/api/v1/roles/",toPath roleTypeOrRoleId,"/subscriptions/",toPath notificationType,"/subscribe"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data SubscribeRoleSubscriptionByNotificationType  
instance Produces SubscribeRoleSubscriptionByNotificationType MimeNoContent


-- *** subscribeUserSubscriptionByNotificationType

-- | @POST \/api\/v1\/users\/{userId}\/subscriptions\/{notificationType}\/subscribe@
-- 
-- Subscribe to a specific notification type
-- 
-- Subscribes a User to a specific notification type. Only the current User can subscribe to a specific notification type. An AccessDeniedException message is sent if requests are made from other users.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
subscribeUserSubscriptionByNotificationType
  :: UserId -- ^ "userId"
  -> NotificationTypeText -- ^ "notificationType"
  -> OktaRequest SubscribeUserSubscriptionByNotificationType MimeNoContent NoContent MimeNoContent
subscribeUserSubscriptionByNotificationType (UserId userId) (NotificationTypeText notificationType) =
  _mkRequest "POST" ["/api/v1/users/",toPath userId,"/subscriptions/",toPath notificationType,"/subscribe"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data SubscribeUserSubscriptionByNotificationType  
instance Produces SubscribeUserSubscriptionByNotificationType MimeNoContent


-- *** unsubscribeRoleSubscriptionByNotificationType

-- | @POST \/api\/v1\/roles\/{roleTypeOrRoleId}\/subscriptions\/{notificationType}\/unsubscribe@
-- 
-- Unsubscribe a Custom Role from a specific notification type
-- 
-- When roleType Unsubscribes a Role from a specific notification type. When you change the subscription status of a Role, it overrides the subscription of any individual user of that Role. Else when roleId Unsubscribes a Custom Role from a specific notification type. When you change the subscription status of a Custom Role, it overrides the subscription of any individual user of that Custom Role.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
unsubscribeRoleSubscriptionByNotificationType
  :: RoleTypeOrRoleId -- ^ "roleTypeOrRoleId"
  -> NotificationTypeText -- ^ "notificationType"
  -> OktaRequest UnsubscribeRoleSubscriptionByNotificationType MimeNoContent NoContent MimeNoContent
unsubscribeRoleSubscriptionByNotificationType (RoleTypeOrRoleId roleTypeOrRoleId) (NotificationTypeText notificationType) =
  _mkRequest "POST" ["/api/v1/roles/",toPath roleTypeOrRoleId,"/subscriptions/",toPath notificationType,"/unsubscribe"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data UnsubscribeRoleSubscriptionByNotificationType  
instance Produces UnsubscribeRoleSubscriptionByNotificationType MimeNoContent


-- *** unsubscribeUserSubscriptionByNotificationType

-- | @POST \/api\/v1\/users\/{userId}\/subscriptions\/{notificationType}\/unsubscribe@
-- 
-- Unsubscribe from a specific notification type
-- 
-- Unsubscribes a User from a specific notification type. Only the current User can unsubscribe from a specific notification type. An AccessDeniedException message is sent if requests are made from other users.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
unsubscribeUserSubscriptionByNotificationType
  :: UserId -- ^ "userId"
  -> NotificationTypeText -- ^ "notificationType"
  -> OktaRequest UnsubscribeUserSubscriptionByNotificationType MimeNoContent NoContent MimeNoContent
unsubscribeUserSubscriptionByNotificationType (UserId userId) (NotificationTypeText notificationType) =
  _mkRequest "POST" ["/api/v1/users/",toPath userId,"/subscriptions/",toPath notificationType,"/unsubscribe"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data UnsubscribeUserSubscriptionByNotificationType  
instance Produces UnsubscribeUserSubscriptionByNotificationType MimeNoContent
