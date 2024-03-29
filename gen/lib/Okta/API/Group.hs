{-
   Okta API

   Allows customers to easily access the Okta API

   OpenAPI Version: 3.0.1
   Okta API API version: 1.0.0
   Contact: devex-public@okta.com
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Okta.API.Group
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Okta.API.Group where

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


-- ** Group

-- *** activateGroupRule

-- | @POST \/api\/v1\/groups\/rules\/{ruleId}\/lifecycle\/activate@
-- 
-- Activate a group Rule
-- 
-- Activates a specific group rule by id from your organization
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
activateGroupRule
  :: RuleId -- ^ "ruleId"
  -> OktaRequest ActivateGroupRule MimeNoContent NoContent MimeNoContent
activateGroupRule (RuleId ruleId) =
  _mkRequest "POST" ["/api/v1/groups/rules/",toPath ruleId,"/lifecycle/activate"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ActivateGroupRule  
instance Produces ActivateGroupRule MimeNoContent


-- *** addApplicationInstanceTargetToAppAdminRoleGivenToGroup

-- | @PUT \/api\/v1\/groups\/{groupId}\/roles\/{roleId}\/targets\/catalog\/apps\/{appName}\/{applicationId}@
-- 
-- Add App Instance Target to App Administrator Role given to a Group
-- 
-- Add App Instance Target to App Administrator Role given to a Group
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
addApplicationInstanceTargetToAppAdminRoleGivenToGroup
  :: GroupId -- ^ "groupId"
  -> RoleId -- ^ "roleId"
  -> AppName -- ^ "appName"
  -> ApplicationId -- ^ "applicationId"
  -> OktaRequest AddApplicationInstanceTargetToAppAdminRoleGivenToGroup MimeNoContent NoContent MimeNoContent
addApplicationInstanceTargetToAppAdminRoleGivenToGroup (GroupId groupId) (RoleId roleId) (AppName appName) (ApplicationId applicationId) =
  _mkRequest "PUT" ["/api/v1/groups/",toPath groupId,"/roles/",toPath roleId,"/targets/catalog/apps/",toPath appName,"/",toPath applicationId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data AddApplicationInstanceTargetToAppAdminRoleGivenToGroup  
instance Produces AddApplicationInstanceTargetToAppAdminRoleGivenToGroup MimeNoContent


-- *** addApplicationTargetToAdminRoleGivenToGroup

-- | @PUT \/api\/v1\/groups\/{groupId}\/roles\/{roleId}\/targets\/catalog\/apps\/{appName}@
-- 
-- Success
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
addApplicationTargetToAdminRoleGivenToGroup
  :: GroupId -- ^ "groupId"
  -> RoleId -- ^ "roleId"
  -> AppName -- ^ "appName"
  -> OktaRequest AddApplicationTargetToAdminRoleGivenToGroup MimeNoContent NoContent MimeNoContent
addApplicationTargetToAdminRoleGivenToGroup (GroupId groupId) (RoleId roleId) (AppName appName) =
  _mkRequest "PUT" ["/api/v1/groups/",toPath groupId,"/roles/",toPath roleId,"/targets/catalog/apps/",toPath appName]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data AddApplicationTargetToAdminRoleGivenToGroup  
instance Produces AddApplicationTargetToAdminRoleGivenToGroup MimeNoContent


-- *** addGroupTargetToGroupAdministratorRoleForGroup

-- | @PUT \/api\/v1\/groups\/{groupId}\/roles\/{roleId}\/targets\/groups\/{targetGroupId}@
-- 
addGroupTargetToGroupAdministratorRoleForGroup
  :: GroupId -- ^ "groupId"
  -> RoleId -- ^ "roleId"
  -> TargetGroupId -- ^ "targetGroupId"
  -> OktaRequest AddGroupTargetToGroupAdministratorRoleForGroup MimeNoContent NoContent MimeNoContent
addGroupTargetToGroupAdministratorRoleForGroup (GroupId groupId) (RoleId roleId) (TargetGroupId targetGroupId) =
  _mkRequest "PUT" ["/api/v1/groups/",toPath groupId,"/roles/",toPath roleId,"/targets/groups/",toPath targetGroupId]

data AddGroupTargetToGroupAdministratorRoleForGroup  
instance Produces AddGroupTargetToGroupAdministratorRoleForGroup MimeNoContent


-- *** addUserToGroup

-- | @PUT \/api\/v1\/groups\/{groupId}\/users\/{userId}@
-- 
-- Add User to Group
-- 
-- Adds a user to a group with 'OKTA_GROUP' type.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
addUserToGroup
  :: GroupId -- ^ "groupId"
  -> UserId -- ^ "userId"
  -> OktaRequest AddUserToGroup MimeNoContent NoContent MimeNoContent
addUserToGroup (GroupId groupId) (UserId userId) =
  _mkRequest "PUT" ["/api/v1/groups/",toPath groupId,"/users/",toPath userId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data AddUserToGroup  
instance Produces AddUserToGroup MimeNoContent


-- *** assignRoleToGroup

-- | @POST \/api\/v1\/groups\/{groupId}\/roles@
-- 
-- Assigns a Role to a Group
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
assignRoleToGroup
  :: (Consumes AssignRoleToGroup MimeJSON, MimeRender MimeJSON AssignRoleRequest)
  => AssignRoleRequest -- ^ "assignRoleRequest"
  -> GroupId -- ^ "groupId"
  -> OktaRequest AssignRoleToGroup MimeJSON Role MimeJSON
assignRoleToGroup assignRoleRequest (GroupId groupId) =
  _mkRequest "POST" ["/api/v1/groups/",toPath groupId,"/roles"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` assignRoleRequest

data AssignRoleToGroup 
instance HasBodyParam AssignRoleToGroup AssignRoleRequest 
instance HasOptionalParam AssignRoleToGroup DisableNotifications where
  applyOptionalParam req (DisableNotifications xs) =
    req `addQuery` toQuery ("disableNotifications", Just xs)

-- | @application/json@
instance Consumes AssignRoleToGroup MimeJSON

-- | @application/json@
instance Produces AssignRoleToGroup MimeJSON


-- *** createGroup

-- | @POST \/api\/v1\/groups@
-- 
-- Add Group
-- 
-- Adds a new group with `OKTA_GROUP` type to your organization.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
createGroup
  :: (Consumes CreateGroup MimeJSON, MimeRender MimeJSON Group)
  => Group -- ^ "group"
  -> OktaRequest CreateGroup MimeJSON Group MimeJSON
createGroup group =
  _mkRequest "POST" ["/api/v1/groups"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` group

data CreateGroup 
instance HasBodyParam CreateGroup Group 

-- | @application/json@
instance Consumes CreateGroup MimeJSON

-- | @application/json@
instance Produces CreateGroup MimeJSON


-- *** createGroupRule

-- | @POST \/api\/v1\/groups\/rules@
-- 
-- Create Group Rule
-- 
-- Creates a group rule to dynamically add users to the specified group if they match the condition
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
createGroupRule
  :: (Consumes CreateGroupRule MimeJSON, MimeRender MimeJSON GroupRule)
  => GroupRule -- ^ "groupRule"
  -> OktaRequest CreateGroupRule MimeJSON GroupRule MimeJSON
createGroupRule groupRule =
  _mkRequest "POST" ["/api/v1/groups/rules"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` groupRule

data CreateGroupRule 
instance HasBodyParam CreateGroupRule GroupRule 

-- | @application/json@
instance Consumes CreateGroupRule MimeJSON

-- | @application/json@
instance Produces CreateGroupRule MimeJSON


-- *** deactivateGroupRule

-- | @POST \/api\/v1\/groups\/rules\/{ruleId}\/lifecycle\/deactivate@
-- 
-- Deactivate a group Rule
-- 
-- Deactivates a specific group rule by id from your organization
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
deactivateGroupRule
  :: RuleId -- ^ "ruleId"
  -> OktaRequest DeactivateGroupRule MimeNoContent NoContent MimeNoContent
deactivateGroupRule (RuleId ruleId) =
  _mkRequest "POST" ["/api/v1/groups/rules/",toPath ruleId,"/lifecycle/deactivate"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data DeactivateGroupRule  
instance Produces DeactivateGroupRule MimeNoContent


-- *** deleteGroup

-- | @DELETE \/api\/v1\/groups\/{groupId}@
-- 
-- Remove Group
-- 
-- Removes a group with `OKTA_GROUP` type from your organization.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
deleteGroup
  :: GroupId -- ^ "groupId"
  -> OktaRequest DeleteGroup MimeNoContent NoContent MimeNoContent
deleteGroup (GroupId groupId) =
  _mkRequest "DELETE" ["/api/v1/groups/",toPath groupId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data DeleteGroup  
instance Produces DeleteGroup MimeNoContent


-- *** deleteGroupRule

-- | @DELETE \/api\/v1\/groups\/rules\/{ruleId}@
-- 
-- Delete a group Rule
-- 
-- Removes a specific group rule by id from your organization
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
deleteGroupRule
  :: RuleId -- ^ "ruleId"
  -> OktaRequest DeleteGroupRule MimeNoContent NoContent MimeNoContent
deleteGroupRule (RuleId ruleId) =
  _mkRequest "DELETE" ["/api/v1/groups/rules/",toPath ruleId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data DeleteGroupRule  

-- | /Optional Param/ "removeUsers" - Indicates whether to keep or remove users from groups assigned by this rule.
instance HasOptionalParam DeleteGroupRule RemoveUsers where
  applyOptionalParam req (RemoveUsers xs) =
    req `addQuery` toQuery ("removeUsers", Just xs)
instance Produces DeleteGroupRule MimeNoContent


-- *** getGroup

-- | @GET \/api\/v1\/groups\/{groupId}@
-- 
-- List Group Rules
-- 
-- Fetches a group from your organization.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
getGroup
  :: GroupId -- ^ "groupId"
  -> OktaRequest GetGroup MimeNoContent Group MimeJSON
getGroup (GroupId groupId) =
  _mkRequest "GET" ["/api/v1/groups/",toPath groupId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data GetGroup  
-- | @application/json@
instance Produces GetGroup MimeJSON


-- *** getGroupRule

-- | @GET \/api\/v1\/groups\/rules\/{ruleId}@
-- 
-- Get Group Rule
-- 
-- Fetches a specific group rule by id from your organization
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
getGroupRule
  :: RuleId -- ^ "ruleId"
  -> OktaRequest GetGroupRule MimeNoContent GroupRule MimeJSON
getGroupRule (RuleId ruleId) =
  _mkRequest "GET" ["/api/v1/groups/rules/",toPath ruleId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data GetGroupRule  
instance HasOptionalParam GetGroupRule Expand where
  applyOptionalParam req (Expand xs) =
    req `addQuery` toQuery ("expand", Just xs)
-- | @application/json@
instance Produces GetGroupRule MimeJSON


-- *** getRole

-- | @GET \/api\/v1\/groups\/{groupId}\/roles\/{roleId}@
-- 
-- Success
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
getRole
  :: GroupId -- ^ "groupId"
  -> RoleId -- ^ "roleId"
  -> OktaRequest GetRole MimeNoContent Role MimeJSON
getRole (GroupId groupId) (RoleId roleId) =
  _mkRequest "GET" ["/api/v1/groups/",toPath groupId,"/roles/",toPath roleId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data GetRole  
-- | @application/json@
instance Produces GetRole MimeJSON


-- *** listApplicationTargetsForApplicationAdministratorRoleForGroup

-- | @GET \/api\/v1\/groups\/{groupId}\/roles\/{roleId}\/targets\/catalog\/apps@
-- 
-- Lists all App targets for an `APP_ADMIN` Role assigned to a Group. This methods return list may include full Applications or Instances. The response for an instance will have an `ID` value, while Application will not have an ID.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
listApplicationTargetsForApplicationAdministratorRoleForGroup
  :: GroupId -- ^ "groupId"
  -> RoleId -- ^ "roleId"
  -> OktaRequest ListApplicationTargetsForApplicationAdministratorRoleForGroup MimeNoContent [CatalogApplication] MimeJSON
listApplicationTargetsForApplicationAdministratorRoleForGroup (GroupId groupId) (RoleId roleId) =
  _mkRequest "GET" ["/api/v1/groups/",toPath groupId,"/roles/",toPath roleId,"/targets/catalog/apps"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ListApplicationTargetsForApplicationAdministratorRoleForGroup  
instance HasOptionalParam ListApplicationTargetsForApplicationAdministratorRoleForGroup After where
  applyOptionalParam req (After xs) =
    req `addQuery` toQuery ("after", Just xs)
instance HasOptionalParam ListApplicationTargetsForApplicationAdministratorRoleForGroup Limit where
  applyOptionalParam req (Limit xs) =
    req `addQuery` toQuery ("limit", Just xs)
-- | @application/json@
instance Produces ListApplicationTargetsForApplicationAdministratorRoleForGroup MimeJSON


-- *** listAssignedApplicationsForGroup

-- | @GET \/api\/v1\/groups\/{groupId}\/apps@
-- 
-- List Assigned Applications
-- 
-- Enumerates all applications that are assigned to a group.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
listAssignedApplicationsForGroup
  :: GroupId -- ^ "groupId"
  -> OktaRequest ListAssignedApplicationsForGroup MimeNoContent [Application] MimeJSON
listAssignedApplicationsForGroup (GroupId groupId) =
  _mkRequest "GET" ["/api/v1/groups/",toPath groupId,"/apps"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ListAssignedApplicationsForGroup  

-- | /Optional Param/ "after" - Specifies the pagination cursor for the next page of apps
instance HasOptionalParam ListAssignedApplicationsForGroup After where
  applyOptionalParam req (After xs) =
    req `addQuery` toQuery ("after", Just xs)

-- | /Optional Param/ "limit" - Specifies the number of app results for a page
instance HasOptionalParam ListAssignedApplicationsForGroup Limit where
  applyOptionalParam req (Limit xs) =
    req `addQuery` toQuery ("limit", Just xs)
-- | @application/json@
instance Produces ListAssignedApplicationsForGroup MimeJSON


-- *** listGroupAssignedRoles

-- | @GET \/api\/v1\/groups\/{groupId}\/roles@
-- 
-- Success
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
listGroupAssignedRoles
  :: GroupId -- ^ "groupId"
  -> OktaRequest ListGroupAssignedRoles MimeNoContent [Role] MimeJSON
listGroupAssignedRoles (GroupId groupId) =
  _mkRequest "GET" ["/api/v1/groups/",toPath groupId,"/roles"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ListGroupAssignedRoles  
instance HasOptionalParam ListGroupAssignedRoles Expand where
  applyOptionalParam req (Expand xs) =
    req `addQuery` toQuery ("expand", Just xs)
-- | @application/json@
instance Produces ListGroupAssignedRoles MimeJSON


-- *** listGroupRules

-- | @GET \/api\/v1\/groups\/rules@
-- 
-- List Group Rules
-- 
-- Lists all group rules for your organization.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
listGroupRules
  :: OktaRequest ListGroupRules MimeNoContent [GroupRule] MimeJSON
listGroupRules =
  _mkRequest "GET" ["/api/v1/groups/rules"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ListGroupRules  

-- | /Optional Param/ "limit" - Specifies the number of rule results in a page
instance HasOptionalParam ListGroupRules Limit where
  applyOptionalParam req (Limit xs) =
    req `addQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "after" - Specifies the pagination cursor for the next page of rules
instance HasOptionalParam ListGroupRules After where
  applyOptionalParam req (After xs) =
    req `addQuery` toQuery ("after", Just xs)

-- | /Optional Param/ "search" - Specifies the keyword to search fules for
instance HasOptionalParam ListGroupRules Search where
  applyOptionalParam req (Search xs) =
    req `addQuery` toQuery ("search", Just xs)

-- | /Optional Param/ "expand" - If specified as `groupIdToGroupNameMap`, then show group names
instance HasOptionalParam ListGroupRules Expand where
  applyOptionalParam req (Expand xs) =
    req `addQuery` toQuery ("expand", Just xs)
-- | @application/json@
instance Produces ListGroupRules MimeJSON


-- *** listGroupTargetsForGroupRole

-- | @GET \/api\/v1\/groups\/{groupId}\/roles\/{roleId}\/targets\/groups@
-- 
-- Success
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
listGroupTargetsForGroupRole
  :: GroupId -- ^ "groupId"
  -> RoleId -- ^ "roleId"
  -> OktaRequest ListGroupTargetsForGroupRole MimeNoContent [Group] MimeJSON
listGroupTargetsForGroupRole (GroupId groupId) (RoleId roleId) =
  _mkRequest "GET" ["/api/v1/groups/",toPath groupId,"/roles/",toPath roleId,"/targets/groups"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ListGroupTargetsForGroupRole  
instance HasOptionalParam ListGroupTargetsForGroupRole After where
  applyOptionalParam req (After xs) =
    req `addQuery` toQuery ("after", Just xs)
instance HasOptionalParam ListGroupTargetsForGroupRole Limit where
  applyOptionalParam req (Limit xs) =
    req `addQuery` toQuery ("limit", Just xs)
-- | @application/json@
instance Produces ListGroupTargetsForGroupRole MimeJSON


-- *** listGroupUsers

-- | @GET \/api\/v1\/groups\/{groupId}\/users@
-- 
-- List Group Members
-- 
-- Enumerates all users that are a member of a group.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
listGroupUsers
  :: GroupId -- ^ "groupId"
  -> OktaRequest ListGroupUsers MimeNoContent [User] MimeJSON
listGroupUsers (GroupId groupId) =
  _mkRequest "GET" ["/api/v1/groups/",toPath groupId,"/users"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ListGroupUsers  

-- | /Optional Param/ "after" - Specifies the pagination cursor for the next page of users
instance HasOptionalParam ListGroupUsers After where
  applyOptionalParam req (After xs) =
    req `addQuery` toQuery ("after", Just xs)

-- | /Optional Param/ "limit" - Specifies the number of user results in a page
instance HasOptionalParam ListGroupUsers Limit where
  applyOptionalParam req (Limit xs) =
    req `addQuery` toQuery ("limit", Just xs)
-- | @application/json@
instance Produces ListGroupUsers MimeJSON


-- *** listGroups

-- | @GET \/api\/v1\/groups@
-- 
-- List Groups
-- 
-- Enumerates groups in your organization with pagination. A subset of groups can be returned that match a supported filter expression or query.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
listGroups
  :: OktaRequest ListGroups MimeNoContent [Group] MimeJSON
listGroups =
  _mkRequest "GET" ["/api/v1/groups"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data ListGroups  

-- | /Optional Param/ "q" - Searches the name property of groups for matching value
instance HasOptionalParam ListGroups Q where
  applyOptionalParam req (Q xs) =
    req `addQuery` toQuery ("q", Just xs)

-- | /Optional Param/ "filter" - Filter expression for groups
instance HasOptionalParam ListGroups Filter where
  applyOptionalParam req (Filter xs) =
    req `addQuery` toQuery ("filter", Just xs)

-- | /Optional Param/ "after" - Specifies the pagination cursor for the next page of groups
instance HasOptionalParam ListGroups After where
  applyOptionalParam req (After xs) =
    req `addQuery` toQuery ("after", Just xs)

-- | /Optional Param/ "limit" - Specifies the number of group results in a page
instance HasOptionalParam ListGroups Limit where
  applyOptionalParam req (Limit xs) =
    req `addQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "expand" - If specified, it causes additional metadata to be included in the response.
instance HasOptionalParam ListGroups Expand where
  applyOptionalParam req (Expand xs) =
    req `addQuery` toQuery ("expand", Just xs)

-- | /Optional Param/ "search" - Searches for groups with a supported filtering expression for all attributes except for _embedded, _links, and objectClass
instance HasOptionalParam ListGroups Search where
  applyOptionalParam req (Search xs) =
    req `addQuery` toQuery ("search", Just xs)
-- | @application/json@
instance Produces ListGroups MimeJSON


-- *** removeApplicationTargetFromAdministratorRoleGivenToGroup

-- | @DELETE \/api\/v1\/groups\/{groupId}\/roles\/{roleId}\/targets\/catalog\/apps\/{appName}\/{applicationId}@
-- 
-- Remove App Instance Target to App Administrator Role given to a Group
-- 
-- Remove App Instance Target to App Administrator Role given to a Group
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
removeApplicationTargetFromAdministratorRoleGivenToGroup
  :: GroupId -- ^ "groupId"
  -> RoleId -- ^ "roleId"
  -> AppName -- ^ "appName"
  -> ApplicationId -- ^ "applicationId"
  -> OktaRequest RemoveApplicationTargetFromAdministratorRoleGivenToGroup MimeNoContent NoContent MimeNoContent
removeApplicationTargetFromAdministratorRoleGivenToGroup (GroupId groupId) (RoleId roleId) (AppName appName) (ApplicationId applicationId) =
  _mkRequest "DELETE" ["/api/v1/groups/",toPath groupId,"/roles/",toPath roleId,"/targets/catalog/apps/",toPath appName,"/",toPath applicationId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data RemoveApplicationTargetFromAdministratorRoleGivenToGroup  
instance Produces RemoveApplicationTargetFromAdministratorRoleGivenToGroup MimeNoContent


-- *** removeApplicationTargetFromApplicationAdministratorRoleGivenToGroup

-- | @DELETE \/api\/v1\/groups\/{groupId}\/roles\/{roleId}\/targets\/catalog\/apps\/{appName}@
-- 
-- Success
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
removeApplicationTargetFromApplicationAdministratorRoleGivenToGroup
  :: GroupId -- ^ "groupId"
  -> RoleId -- ^ "roleId"
  -> AppName -- ^ "appName"
  -> OktaRequest RemoveApplicationTargetFromApplicationAdministratorRoleGivenToGroup MimeNoContent NoContent MimeNoContent
removeApplicationTargetFromApplicationAdministratorRoleGivenToGroup (GroupId groupId) (RoleId roleId) (AppName appName) =
  _mkRequest "DELETE" ["/api/v1/groups/",toPath groupId,"/roles/",toPath roleId,"/targets/catalog/apps/",toPath appName]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data RemoveApplicationTargetFromApplicationAdministratorRoleGivenToGroup  
instance Produces RemoveApplicationTargetFromApplicationAdministratorRoleGivenToGroup MimeNoContent


-- *** removeGroupTargetFromGroupAdministratorRoleGivenToGroup

-- | @DELETE \/api\/v1\/groups\/{groupId}\/roles\/{roleId}\/targets\/groups\/{targetGroupId}@
-- 
removeGroupTargetFromGroupAdministratorRoleGivenToGroup
  :: GroupId -- ^ "groupId"
  -> RoleId -- ^ "roleId"
  -> TargetGroupId -- ^ "targetGroupId"
  -> OktaRequest RemoveGroupTargetFromGroupAdministratorRoleGivenToGroup MimeNoContent NoContent MimeNoContent
removeGroupTargetFromGroupAdministratorRoleGivenToGroup (GroupId groupId) (RoleId roleId) (TargetGroupId targetGroupId) =
  _mkRequest "DELETE" ["/api/v1/groups/",toPath groupId,"/roles/",toPath roleId,"/targets/groups/",toPath targetGroupId]

data RemoveGroupTargetFromGroupAdministratorRoleGivenToGroup  
instance Produces RemoveGroupTargetFromGroupAdministratorRoleGivenToGroup MimeNoContent


-- *** removeRoleFromGroup

-- | @DELETE \/api\/v1\/groups\/{groupId}\/roles\/{roleId}@
-- 
-- Unassigns a Role from a Group
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
removeRoleFromGroup
  :: GroupId -- ^ "groupId"
  -> RoleId -- ^ "roleId"
  -> OktaRequest RemoveRoleFromGroup MimeNoContent NoContent MimeNoContent
removeRoleFromGroup (GroupId groupId) (RoleId roleId) =
  _mkRequest "DELETE" ["/api/v1/groups/",toPath groupId,"/roles/",toPath roleId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data RemoveRoleFromGroup  
instance Produces RemoveRoleFromGroup MimeNoContent


-- *** removeUserFromGroup

-- | @DELETE \/api\/v1\/groups\/{groupId}\/users\/{userId}@
-- 
-- Remove User from Group
-- 
-- Removes a user from a group with 'OKTA_GROUP' type.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
removeUserFromGroup
  :: GroupId -- ^ "groupId"
  -> UserId -- ^ "userId"
  -> OktaRequest RemoveUserFromGroup MimeNoContent NoContent MimeNoContent
removeUserFromGroup (GroupId groupId) (UserId userId) =
  _mkRequest "DELETE" ["/api/v1/groups/",toPath groupId,"/users/",toPath userId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)

data RemoveUserFromGroup  
instance Produces RemoveUserFromGroup MimeNoContent


-- *** updateGroup

-- | @PUT \/api\/v1\/groups\/{groupId}@
-- 
-- Update Group
-- 
-- Updates the profile for a group with `OKTA_GROUP` type from your organization.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
updateGroup
  :: (Consumes UpdateGroup MimeJSON, MimeRender MimeJSON Group)
  => Group -- ^ "group"
  -> GroupId -- ^ "groupId"
  -> OktaRequest UpdateGroup MimeJSON Group MimeJSON
updateGroup group (GroupId groupId) =
  _mkRequest "PUT" ["/api/v1/groups/",toPath groupId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` group

data UpdateGroup 
instance HasBodyParam UpdateGroup Group 

-- | @application/json@
instance Consumes UpdateGroup MimeJSON

-- | @application/json@
instance Produces UpdateGroup MimeJSON


-- *** updateGroupRule

-- | @PUT \/api\/v1\/groups\/rules\/{ruleId}@
-- 
-- Updates a group rule. Only `INACTIVE` rules can be updated.
-- 
-- AuthMethod: 'AuthApiKeyApiToken'
-- 
updateGroupRule
  :: (Consumes UpdateGroupRule MimeJSON, MimeRender MimeJSON GroupRule)
  => GroupRule -- ^ "groupRule"
  -> RuleId -- ^ "ruleId"
  -> OktaRequest UpdateGroupRule MimeJSON GroupRule MimeJSON
updateGroupRule groupRule (RuleId ruleId) =
  _mkRequest "PUT" ["/api/v1/groups/rules/",toPath ruleId]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiToken)
    `setBodyParam` groupRule

data UpdateGroupRule 
instance HasBodyParam UpdateGroupRule GroupRule 

-- | @application/json@
instance Consumes UpdateGroupRule MimeJSON

-- | @application/json@
instance Produces UpdateGroupRule MimeJSON

