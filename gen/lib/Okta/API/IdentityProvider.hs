{-
   Okta API

   Allows customers to easily access the Okta API

   OpenAPI Version: 3.0.1
   Okta API API version: 1.0.0
   Contact: devex-public@okta.com
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Okta.API.IdentityProvider
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Okta.API.IdentityProvider where

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


-- ** IdentityProvider

-- *** activateIdentityProvider

-- | @POST \/api\/v1\/idps\/{idpId}\/lifecycle\/activate@
-- 
-- Activate Identity Provider
-- 
-- Activates an inactive IdP.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
activateIdentityProvider
  :: IdpId -- ^ "idpId"
  -> OktaRequest ActivateIdentityProvider MimeNoContent IdentityProvider MimeJSON
activateIdentityProvider (IdpId idpId) =
  _mkRequest "POST" ["/api/v1/idps/",toPath idpId,"/lifecycle/activate"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ActivateIdentityProvider  
-- | @application/json@
instance Produces ActivateIdentityProvider MimeJSON


-- *** apiV1IdpsIdpIdCredentialsCsrsCsrIdLifecyclePublishPost

-- | @POST \/api\/v1\/idps\/{idpId}\/credentials\/csrs\/{csrId}\/lifecycle\/publish@
-- 
-- Update the Certificate Signing Request with a signed X.509 certificate and add it into the signing key credentials for the IdP.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
apiV1IdpsIdpIdCredentialsCsrsCsrIdLifecyclePublishPost
  :: IdpId -- ^ "idpId"
  -> CsrId -- ^ "csrId"
  -> OktaRequest ApiV1IdpsIdpIdCredentialsCsrsCsrIdLifecyclePublishPost MimeNoContent JsonWebKey MimeJSON
apiV1IdpsIdpIdCredentialsCsrsCsrIdLifecyclePublishPost (IdpId idpId) (CsrId csrId) =
  _mkRequest "POST" ["/api/v1/idps/",toPath idpId,"/credentials/csrs/",toPath csrId,"/lifecycle/publish"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ApiV1IdpsIdpIdCredentialsCsrsCsrIdLifecyclePublishPost  
-- | @application/json@
instance Produces ApiV1IdpsIdpIdCredentialsCsrsCsrIdLifecyclePublishPost MimeJSON


-- *** cloneIdentityProviderKey

-- | @POST \/api\/v1\/idps\/{idpId}\/credentials\/keys\/{keyId}\/clone@
-- 
-- Clone Signing Key Credential for IdP
-- 
-- Clones a X.509 certificate for an IdP signing key credential from a source IdP to target IdP
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
cloneIdentityProviderKey
  :: IdpId -- ^ "idpId"
  -> KeyId -- ^ "keyId"
  -> TargetIdpId -- ^ "targetIdpId"
  -> OktaRequest CloneIdentityProviderKey MimeNoContent JsonWebKey MimeJSON
cloneIdentityProviderKey (IdpId idpId) (KeyId keyId) (TargetIdpId targetIdpId) =
  _mkRequest "POST" ["/api/v1/idps/",toPath idpId,"/credentials/keys/",toPath keyId,"/clone"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `addQuery` toQuery ("targetIdpId", Just targetIdpId)

data CloneIdentityProviderKey  
-- | @application/json@
instance Produces CloneIdentityProviderKey MimeJSON


-- *** createIdentityProvider

-- | @POST \/api\/v1\/idps@
-- 
-- Add Identity Provider
-- 
-- Adds a new IdP to your organization.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
createIdentityProvider
  :: (Consumes CreateIdentityProvider MimeJSON, MimeRender MimeJSON IdentityProvider)
  => IdentityProvider -- ^ "identityProvider"
  -> OktaRequest CreateIdentityProvider MimeJSON IdentityProvider MimeJSON
createIdentityProvider identityProvider =
  _mkRequest "POST" ["/api/v1/idps"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` identityProvider

data CreateIdentityProvider 
instance HasBodyParam CreateIdentityProvider IdentityProvider 

-- | @application/json@
instance Consumes CreateIdentityProvider MimeJSON

-- | @application/json@
instance Produces CreateIdentityProvider MimeJSON


-- *** createIdentityProviderKey

-- | @POST \/api\/v1\/idps\/credentials\/keys@
-- 
-- Add X.509 Certificate Public Key
-- 
-- Adds a new X.509 certificate credential to the IdP key store.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
createIdentityProviderKey
  :: (Consumes CreateIdentityProviderKey MimeJSON, MimeRender MimeJSON JsonWebKey)
  => JsonWebKey -- ^ "jsonWebKey"
  -> OktaRequest CreateIdentityProviderKey MimeJSON JsonWebKey MimeJSON
createIdentityProviderKey jsonWebKey =
  _mkRequest "POST" ["/api/v1/idps/credentials/keys"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` jsonWebKey

data CreateIdentityProviderKey 
instance HasBodyParam CreateIdentityProviderKey JsonWebKey 

-- | @application/json@
instance Consumes CreateIdentityProviderKey MimeJSON

-- | @application/json@
instance Produces CreateIdentityProviderKey MimeJSON


-- *** deactivateIdentityProvider

-- | @POST \/api\/v1\/idps\/{idpId}\/lifecycle\/deactivate@
-- 
-- Deactivate Identity Provider
-- 
-- Deactivates an active IdP.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
deactivateIdentityProvider
  :: IdpId -- ^ "idpId"
  -> OktaRequest DeactivateIdentityProvider MimeNoContent IdentityProvider MimeJSON
deactivateIdentityProvider (IdpId idpId) =
  _mkRequest "POST" ["/api/v1/idps/",toPath idpId,"/lifecycle/deactivate"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data DeactivateIdentityProvider  
-- | @application/json@
instance Produces DeactivateIdentityProvider MimeJSON


-- *** deleteIdentityProvider

-- | @DELETE \/api\/v1\/idps\/{idpId}@
-- 
-- Delete Identity Provider
-- 
-- Removes an IdP from your organization.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
deleteIdentityProvider
  :: IdpId -- ^ "idpId"
  -> OktaRequest DeleteIdentityProvider MimeNoContent NoContent MimeNoContent
deleteIdentityProvider (IdpId idpId) =
  _mkRequest "DELETE" ["/api/v1/idps/",toPath idpId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data DeleteIdentityProvider  
instance Produces DeleteIdentityProvider MimeNoContent


-- *** deleteIdentityProviderKey

-- | @DELETE \/api\/v1\/idps\/credentials\/keys\/{keyId}@
-- 
-- Delete Key
-- 
-- Deletes a specific IdP Key Credential by `kid` if it is not currently being used by an Active or Inactive IdP.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
deleteIdentityProviderKey
  :: KeyId -- ^ "keyId"
  -> OktaRequest DeleteIdentityProviderKey MimeNoContent NoContent MimeNoContent
deleteIdentityProviderKey (KeyId keyId) =
  _mkRequest "DELETE" ["/api/v1/idps/credentials/keys/",toPath keyId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data DeleteIdentityProviderKey  
instance Produces DeleteIdentityProviderKey MimeNoContent


-- *** generateCsrForIdentityProvider

-- | @POST \/api\/v1\/idps\/{idpId}\/credentials\/csrs@
-- 
-- Generate Certificate Signing Request for IdP
-- 
-- Generates a new key pair and returns a Certificate Signing Request for it.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
generateCsrForIdentityProvider
  :: (Consumes GenerateCsrForIdentityProvider MimeJSON, MimeRender MimeJSON CsrMetadata)
  => CsrMetadata -- ^ "metadata"
  -> IdpId -- ^ "idpId"
  -> OktaRequest GenerateCsrForIdentityProvider MimeJSON Csr MimeJSON
generateCsrForIdentityProvider metadata (IdpId idpId) =
  _mkRequest "POST" ["/api/v1/idps/",toPath idpId,"/credentials/csrs"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` metadata

data GenerateCsrForIdentityProvider 
instance HasBodyParam GenerateCsrForIdentityProvider CsrMetadata 

-- | @application/json@
instance Consumes GenerateCsrForIdentityProvider MimeJSON

-- | @application/json@
instance Produces GenerateCsrForIdentityProvider MimeJSON


-- *** generateIdentityProviderSigningKey

-- | @POST \/api\/v1\/idps\/{idpId}\/credentials\/keys\/generate@
-- 
-- Generate New IdP Signing Key Credential
-- 
-- Generates a new X.509 certificate for an IdP signing key credential to be used for signing assertions sent to the IdP
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
generateIdentityProviderSigningKey
  :: IdpId -- ^ "idpId"
  -> ValidityYears -- ^ "validityYears" -  expiry of the IdP Key Credential
  -> OktaRequest GenerateIdentityProviderSigningKey MimeNoContent JsonWebKey MimeJSON
generateIdentityProviderSigningKey (IdpId idpId) (ValidityYears validityYears) =
  _mkRequest "POST" ["/api/v1/idps/",toPath idpId,"/credentials/keys/generate"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `addQuery` toQuery ("validityYears", Just validityYears)

data GenerateIdentityProviderSigningKey  
-- | @application/json@
instance Produces GenerateIdentityProviderSigningKey MimeJSON


-- *** getCsrForIdentityProvider

-- | @GET \/api\/v1\/idps\/{idpId}\/credentials\/csrs\/{csrId}@
-- 
-- Gets a specific Certificate Signing Request model by id
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
getCsrForIdentityProvider
  :: IdpId -- ^ "idpId"
  -> CsrId -- ^ "csrId"
  -> OktaRequest GetCsrForIdentityProvider MimeNoContent Csr MimeJSON
getCsrForIdentityProvider (IdpId idpId) (CsrId csrId) =
  _mkRequest "GET" ["/api/v1/idps/",toPath idpId,"/credentials/csrs/",toPath csrId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data GetCsrForIdentityProvider  
-- | @application/json@
instance Produces GetCsrForIdentityProvider MimeJSON


-- *** getIdentityProvider

-- | @GET \/api\/v1\/idps\/{idpId}@
-- 
-- Get Identity Provider
-- 
-- Fetches an IdP by `id`.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
getIdentityProvider
  :: IdpId -- ^ "idpId"
  -> OktaRequest GetIdentityProvider MimeNoContent IdentityProvider MimeJSON
getIdentityProvider (IdpId idpId) =
  _mkRequest "GET" ["/api/v1/idps/",toPath idpId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data GetIdentityProvider  
-- | @application/json@
instance Produces GetIdentityProvider MimeJSON


-- *** getIdentityProviderApplicationUser

-- | @GET \/api\/v1\/idps\/{idpId}\/users\/{userId}@
-- 
-- Fetches a linked IdP user by ID
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
getIdentityProviderApplicationUser
  :: IdpId -- ^ "idpId"
  -> UserId -- ^ "userId"
  -> OktaRequest GetIdentityProviderApplicationUser MimeNoContent IdentityProviderApplicationUser MimeJSON
getIdentityProviderApplicationUser (IdpId idpId) (UserId userId) =
  _mkRequest "GET" ["/api/v1/idps/",toPath idpId,"/users/",toPath userId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data GetIdentityProviderApplicationUser  
-- | @application/json@
instance Produces GetIdentityProviderApplicationUser MimeJSON


-- *** getIdentityProviderKey

-- | @GET \/api\/v1\/idps\/credentials\/keys\/{keyId}@
-- 
-- Get Key
-- 
-- Gets a specific IdP Key Credential by `kid`
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
getIdentityProviderKey
  :: KeyId -- ^ "keyId"
  -> OktaRequest GetIdentityProviderKey MimeNoContent JsonWebKey MimeJSON
getIdentityProviderKey (KeyId keyId) =
  _mkRequest "GET" ["/api/v1/idps/credentials/keys/",toPath keyId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data GetIdentityProviderKey  
-- | @application/json@
instance Produces GetIdentityProviderKey MimeJSON


-- *** getIdentityProviderSigningKey

-- | @GET \/api\/v1\/idps\/{idpId}\/credentials\/keys\/{keyId}@
-- 
-- Get Signing Key Credential for IdP
-- 
-- Gets a specific IdP Key Credential by `kid`
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
getIdentityProviderSigningKey
  :: IdpId -- ^ "idpId"
  -> KeyId -- ^ "keyId"
  -> OktaRequest GetIdentityProviderSigningKey MimeNoContent JsonWebKey MimeJSON
getIdentityProviderSigningKey (IdpId idpId) (KeyId keyId) =
  _mkRequest "GET" ["/api/v1/idps/",toPath idpId,"/credentials/keys/",toPath keyId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data GetIdentityProviderSigningKey  
-- | @application/json@
instance Produces GetIdentityProviderSigningKey MimeJSON


-- *** linkUserToIdentityProvider

-- | @POST \/api\/v1\/idps\/{idpId}\/users\/{userId}@
-- 
-- Link a user to a Social IdP without a transaction
-- 
-- Links an Okta user to an existing Social Identity Provider. This does not support the SAML2 Identity Provider Type
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
linkUserToIdentityProvider
  :: (Consumes LinkUserToIdentityProvider MimeJSON, MimeRender MimeJSON UserIdentityProviderLinkRequest)
  => UserIdentityProviderLinkRequest -- ^ "userIdentityProviderLinkRequest"
  -> IdpId -- ^ "idpId"
  -> UserId -- ^ "userId"
  -> OktaRequest LinkUserToIdentityProvider MimeJSON IdentityProviderApplicationUser MimeJSON
linkUserToIdentityProvider userIdentityProviderLinkRequest (IdpId idpId) (UserId userId) =
  _mkRequest "POST" ["/api/v1/idps/",toPath idpId,"/users/",toPath userId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` userIdentityProviderLinkRequest

data LinkUserToIdentityProvider 
instance HasBodyParam LinkUserToIdentityProvider UserIdentityProviderLinkRequest 

-- | @application/json@
instance Consumes LinkUserToIdentityProvider MimeJSON

-- | @application/json@
instance Produces LinkUserToIdentityProvider MimeJSON


-- *** listCsrsForIdentityProvider

-- | @GET \/api\/v1\/idps\/{idpId}\/credentials\/csrs@
-- 
-- List Certificate Signing Requests for IdP
-- 
-- Enumerates Certificate Signing Requests for an IdP
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
listCsrsForIdentityProvider
  :: IdpId -- ^ "idpId"
  -> OktaRequest ListCsrsForIdentityProvider MimeNoContent [Csr] MimeJSON
listCsrsForIdentityProvider (IdpId idpId) =
  _mkRequest "GET" ["/api/v1/idps/",toPath idpId,"/credentials/csrs"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ListCsrsForIdentityProvider  
-- | @application/json@
instance Produces ListCsrsForIdentityProvider MimeJSON


-- *** listIdentityProviderApplicationUsers

-- | @GET \/api\/v1\/idps\/{idpId}\/users@
-- 
-- Find Users
-- 
-- Find all the users linked to an identity provider
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
listIdentityProviderApplicationUsers
  :: IdpId -- ^ "idpId"
  -> OktaRequest ListIdentityProviderApplicationUsers MimeNoContent [IdentityProviderApplicationUser] MimeJSON
listIdentityProviderApplicationUsers (IdpId idpId) =
  _mkRequest "GET" ["/api/v1/idps/",toPath idpId,"/users"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ListIdentityProviderApplicationUsers  
-- | @application/json@
instance Produces ListIdentityProviderApplicationUsers MimeJSON


-- *** listIdentityProviderKeys

-- | @GET \/api\/v1\/idps\/credentials\/keys@
-- 
-- List Keys
-- 
-- Enumerates IdP key credentials.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
listIdentityProviderKeys
  :: OktaRequest ListIdentityProviderKeys MimeNoContent [JsonWebKey] MimeJSON
listIdentityProviderKeys =
  _mkRequest "GET" ["/api/v1/idps/credentials/keys"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ListIdentityProviderKeys  

-- | /Optional Param/ "after" - Specifies the pagination cursor for the next page of keys
instance HasOptionalParam ListIdentityProviderKeys After where
  applyOptionalParam req (After xs) =
    req `addQuery` toQuery ("after", Just xs)

-- | /Optional Param/ "limit" - Specifies the number of key results in a page
instance HasOptionalParam ListIdentityProviderKeys Limit where
  applyOptionalParam req (Limit xs) =
    req `addQuery` toQuery ("limit", Just xs)
-- | @application/json@
instance Produces ListIdentityProviderKeys MimeJSON


-- *** listIdentityProviderSigningKeys

-- | @GET \/api\/v1\/idps\/{idpId}\/credentials\/keys@
-- 
-- List Signing Key Credentials for IdP
-- 
-- Enumerates signing key credentials for an IdP
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
listIdentityProviderSigningKeys
  :: IdpId -- ^ "idpId"
  -> OktaRequest ListIdentityProviderSigningKeys MimeNoContent [JsonWebKey] MimeJSON
listIdentityProviderSigningKeys (IdpId idpId) =
  _mkRequest "GET" ["/api/v1/idps/",toPath idpId,"/credentials/keys"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ListIdentityProviderSigningKeys  
-- | @application/json@
instance Produces ListIdentityProviderSigningKeys MimeJSON


-- *** listIdentityProviders

-- | @GET \/api\/v1\/idps@
-- 
-- List Identity Providers
-- 
-- Enumerates IdPs in your organization with pagination. A subset of IdPs can be returned that match a supported filter expression or query.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
listIdentityProviders
  :: OktaRequest ListIdentityProviders MimeNoContent [IdentityProvider] MimeJSON
listIdentityProviders =
  _mkRequest "GET" ["/api/v1/idps"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ListIdentityProviders  

-- | /Optional Param/ "q" - Searches the name property of IdPs for matching value
instance HasOptionalParam ListIdentityProviders Q where
  applyOptionalParam req (Q xs) =
    req `addQuery` toQuery ("q", Just xs)

-- | /Optional Param/ "after" - Specifies the pagination cursor for the next page of IdPs
instance HasOptionalParam ListIdentityProviders After where
  applyOptionalParam req (After xs) =
    req `addQuery` toQuery ("after", Just xs)

-- | /Optional Param/ "limit" - Specifies the number of IdP results in a page
instance HasOptionalParam ListIdentityProviders Limit where
  applyOptionalParam req (Limit xs) =
    req `addQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "type" - Filters IdPs by type
instance HasOptionalParam ListIdentityProviders ParamType where
  applyOptionalParam req (ParamType xs) =
    req `addQuery` toQuery ("type", Just xs)
-- | @application/json@
instance Produces ListIdentityProviders MimeJSON


-- *** listSocialAuthTokens

-- | @GET \/api\/v1\/idps\/{idpId}\/users\/{userId}\/credentials\/tokens@
-- 
-- Social Authentication Token Operation
-- 
-- Fetches the tokens minted by the Social Authentication Provider when the user authenticates with Okta via Social Auth.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
listSocialAuthTokens
  :: IdpId -- ^ "idpId"
  -> UserId -- ^ "userId"
  -> OktaRequest ListSocialAuthTokens MimeNoContent [SocialAuthToken] MimeJSON
listSocialAuthTokens (IdpId idpId) (UserId userId) =
  _mkRequest "GET" ["/api/v1/idps/",toPath idpId,"/users/",toPath userId,"/credentials/tokens"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ListSocialAuthTokens  
-- | @application/json@
instance Produces ListSocialAuthTokens MimeJSON


-- *** revokeCsrForIdentityProvider

-- | @DELETE \/api\/v1\/idps\/{idpId}\/credentials\/csrs\/{csrId}@
-- 
-- Revoke a Certificate Signing Request and delete the key pair from the IdP
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
revokeCsrForIdentityProvider
  :: IdpId -- ^ "idpId"
  -> CsrId -- ^ "csrId"
  -> OktaRequest RevokeCsrForIdentityProvider MimeNoContent NoContent MimeNoContent
revokeCsrForIdentityProvider (IdpId idpId) (CsrId csrId) =
  _mkRequest "DELETE" ["/api/v1/idps/",toPath idpId,"/credentials/csrs/",toPath csrId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data RevokeCsrForIdentityProvider  
instance Produces RevokeCsrForIdentityProvider MimeNoContent


-- *** unlinkUserFromIdentityProvider

-- | @DELETE \/api\/v1\/idps\/{idpId}\/users\/{userId}@
-- 
-- Unlink User from IdP
-- 
-- Removes the link between the Okta user and the IdP user.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
unlinkUserFromIdentityProvider
  :: IdpId -- ^ "idpId"
  -> UserId -- ^ "userId"
  -> OktaRequest UnlinkUserFromIdentityProvider MimeNoContent NoContent MimeNoContent
unlinkUserFromIdentityProvider (IdpId idpId) (UserId userId) =
  _mkRequest "DELETE" ["/api/v1/idps/",toPath idpId,"/users/",toPath userId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data UnlinkUserFromIdentityProvider  
instance Produces UnlinkUserFromIdentityProvider MimeNoContent


-- *** updateIdentityProvider

-- | @PUT \/api\/v1\/idps\/{idpId}@
-- 
-- Update Identity Provider
-- 
-- Updates the configuration for an IdP.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
updateIdentityProvider
  :: (Consumes UpdateIdentityProvider MimeJSON, MimeRender MimeJSON IdentityProvider)
  => IdentityProvider -- ^ "identityProvider"
  -> IdpId -- ^ "idpId"
  -> OktaRequest UpdateIdentityProvider MimeJSON IdentityProvider MimeJSON
updateIdentityProvider identityProvider (IdpId idpId) =
  _mkRequest "PUT" ["/api/v1/idps/",toPath idpId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` identityProvider

data UpdateIdentityProvider 
instance HasBodyParam UpdateIdentityProvider IdentityProvider 

-- | @application/json@
instance Consumes UpdateIdentityProvider MimeJSON

-- | @application/json@
instance Produces UpdateIdentityProvider MimeJSON
