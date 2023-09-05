{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import Okta.Model
import Okta.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V
import Data.String (fromString)

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

#if MIN_VERSION_aeson(2,0,0)
#else
-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = arbitraryValue
#endif

arbitraryValue :: Gen A.Value
arbitraryValue =
  frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (fromString k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models

instance Arbitrary AccessPolicyConstraint where
  arbitrary = sized genAccessPolicyConstraint

genAccessPolicyConstraint :: Int -> Gen AccessPolicyConstraint
genAccessPolicyConstraint n =
  AccessPolicyConstraint
    <$> arbitraryReducedMaybe n -- accessPolicyConstraintTypes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- accessPolicyConstraintMethods :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- accessPolicyConstraintReauthenticateIn :: Maybe Text
  
instance Arbitrary AccessPolicyConstraints where
  arbitrary = sized genAccessPolicyConstraints

genAccessPolicyConstraints :: Int -> Gen AccessPolicyConstraints
genAccessPolicyConstraints n =
  AccessPolicyConstraints
    <$> arbitraryReducedMaybeValue n -- accessPolicyConstraintsKnowledge :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- accessPolicyConstraintsPossession :: Maybe PossessionConstraint
  
instance Arbitrary AccessPolicyRule where
  arbitrary = sized genAccessPolicyRule

genAccessPolicyRule :: Int -> Gen AccessPolicyRule
genAccessPolicyRule n =
  AccessPolicyRule
    <$> arbitraryReducedMaybe n -- accessPolicyRuleActions :: Maybe AccessPolicyRuleActions
    <*> arbitraryReducedMaybe n -- accessPolicyRuleConditions :: Maybe AccessPolicyRuleConditions
    <*> arbitraryReducedMaybe n -- accessPolicyRuleName :: Maybe Text
  
instance Arbitrary AccessPolicyRuleActions where
  arbitrary = sized genAccessPolicyRuleActions

genAccessPolicyRuleActions :: Int -> Gen AccessPolicyRuleActions
genAccessPolicyRuleActions n =
  AccessPolicyRuleActions
    <$> arbitraryReducedMaybe n -- accessPolicyRuleActionsAppSignOn :: Maybe AccessPolicyRuleApplicationSignOn
  
instance Arbitrary AccessPolicyRuleApplicationSignOn where
  arbitrary = sized genAccessPolicyRuleApplicationSignOn

genAccessPolicyRuleApplicationSignOn :: Int -> Gen AccessPolicyRuleApplicationSignOn
genAccessPolicyRuleApplicationSignOn n =
  AccessPolicyRuleApplicationSignOn
    <$> arbitraryReducedMaybe n -- accessPolicyRuleApplicationSignOnAccess :: Maybe Text
    <*> arbitraryReducedMaybe n -- accessPolicyRuleApplicationSignOnVerificationMethod :: Maybe VerificationMethod
  
instance Arbitrary AccessPolicyRuleConditions where
  arbitrary = sized genAccessPolicyRuleConditions

genAccessPolicyRuleConditions :: Int -> Gen AccessPolicyRuleConditions
genAccessPolicyRuleConditions n =
  AccessPolicyRuleConditions
    <$> arbitraryReducedMaybe n -- accessPolicyRuleConditionsUserType :: Maybe UserTypeCondition
    <*> arbitraryReducedMaybe n -- accessPolicyRuleConditionsElCondition :: Maybe AccessPolicyRuleCustomCondition
    <*> arbitraryReducedMaybe n -- accessPolicyRuleConditionsDevice :: Maybe DeviceAccessPolicyRuleCondition
  
instance Arbitrary AccessPolicyRuleCustomCondition where
  arbitrary = sized genAccessPolicyRuleCustomCondition

genAccessPolicyRuleCustomCondition :: Int -> Gen AccessPolicyRuleCustomCondition
genAccessPolicyRuleCustomCondition n =
  AccessPolicyRuleCustomCondition
    <$> arbitraryReducedMaybe n -- accessPolicyRuleCustomConditionCondition :: Maybe Text
  
instance Arbitrary AcsEndpoint where
  arbitrary = sized genAcsEndpoint

genAcsEndpoint :: Int -> Gen AcsEndpoint
genAcsEndpoint n =
  AcsEndpoint
    <$> arbitraryReducedMaybe n -- acsEndpointUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- acsEndpointIndex :: Maybe Int
  
instance Arbitrary ActivateFactorRequest where
  arbitrary = sized genActivateFactorRequest

genActivateFactorRequest :: Int -> Gen ActivateFactorRequest
genActivateFactorRequest n =
  ActivateFactorRequest
    <$> arbitraryReducedMaybe n -- activateFactorRequestAttestation :: Maybe Text
    <*> arbitraryReducedMaybe n -- activateFactorRequestClientData :: Maybe Text
    <*> arbitraryReducedMaybe n -- activateFactorRequestPassCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- activateFactorRequestRegistrationData :: Maybe Text
    <*> arbitraryReducedMaybe n -- activateFactorRequestStateToken :: Maybe Text
  
instance Arbitrary AppAndInstanceConditionEvaluatorAppOrInstance where
  arbitrary = sized genAppAndInstanceConditionEvaluatorAppOrInstance

genAppAndInstanceConditionEvaluatorAppOrInstance :: Int -> Gen AppAndInstanceConditionEvaluatorAppOrInstance
genAppAndInstanceConditionEvaluatorAppOrInstance n =
  AppAndInstanceConditionEvaluatorAppOrInstance
    <$> arbitraryReducedMaybe n -- appAndInstanceConditionEvaluatorAppOrInstanceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- appAndInstanceConditionEvaluatorAppOrInstanceName :: Maybe Text
    <*> arbitraryReducedMaybe n -- appAndInstanceConditionEvaluatorAppOrInstanceType :: Maybe E'Type
  
instance Arbitrary AppAndInstancePolicyRuleCondition where
  arbitrary = sized genAppAndInstancePolicyRuleCondition

genAppAndInstancePolicyRuleCondition :: Int -> Gen AppAndInstancePolicyRuleCondition
genAppAndInstancePolicyRuleCondition n =
  AppAndInstancePolicyRuleCondition
    <$> arbitraryReducedMaybe n -- appAndInstancePolicyRuleConditionExclude :: Maybe [AppAndInstanceConditionEvaluatorAppOrInstance]
    <*> arbitraryReducedMaybe n -- appAndInstancePolicyRuleConditionInclude :: Maybe [AppAndInstanceConditionEvaluatorAppOrInstance]
  
instance Arbitrary AppInstancePolicyRuleCondition where
  arbitrary = sized genAppInstancePolicyRuleCondition

genAppInstancePolicyRuleCondition :: Int -> Gen AppInstancePolicyRuleCondition
genAppInstancePolicyRuleCondition n =
  AppInstancePolicyRuleCondition
    <$> arbitraryReducedMaybe n -- appInstancePolicyRuleConditionExclude :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- appInstancePolicyRuleConditionInclude :: Maybe [Text]
  
instance Arbitrary AppLink where
  arbitrary = sized genAppLink

genAppLink :: Int -> Gen AppLink
genAppLink n =
  AppLink
    <$> arbitraryReducedMaybe n -- appLinkAppAssignmentId :: Maybe Text
    <*> arbitraryReducedMaybe n -- appLinkAppInstanceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- appLinkAppName :: Maybe Text
    <*> arbitraryReducedMaybe n -- appLinkCredentialsSetup :: Maybe Bool
    <*> arbitraryReducedMaybe n -- appLinkHidden :: Maybe Bool
    <*> arbitraryReducedMaybe n -- appLinkId :: Maybe Text
    <*> arbitraryReducedMaybe n -- appLinkLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- appLinkLinkUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- appLinkLogoUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- appLinkSortOrder :: Maybe Int
  
instance Arbitrary AppUser where
  arbitrary = sized genAppUser

genAppUser :: Int -> Gen AppUser
genAppUser n =
  AppUser
    <$> arbitraryReducedMaybe n -- appUserEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- appUserLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- appUserCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- appUserCredentials :: Maybe AppUserCredentials
    <*> arbitraryReducedMaybe n -- appUserExternalId :: Maybe Text
    <*> arbitraryReducedMaybe n -- appUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- appUserLastSync :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- appUserLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- appUserPasswordChanged :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- appUserProfile :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- appUserScope :: Maybe Text
    <*> arbitraryReducedMaybe n -- appUserStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- appUserStatusChanged :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- appUserSyncState :: Maybe Text
  
instance Arbitrary AppUserCredentials where
  arbitrary = sized genAppUserCredentials

genAppUserCredentials :: Int -> Gen AppUserCredentials
genAppUserCredentials n =
  AppUserCredentials
    <$> arbitraryReducedMaybe n -- appUserCredentialsPassword :: Maybe AppUserPasswordCredential
    <*> arbitraryReducedMaybe n -- appUserCredentialsUserName :: Maybe Text
  
instance Arbitrary AppUserPasswordCredential where
  arbitrary = sized genAppUserPasswordCredential

genAppUserPasswordCredential :: Int -> Gen AppUserPasswordCredential
genAppUserPasswordCredential n =
  AppUserPasswordCredential
    <$> arbitraryReducedMaybe n -- appUserPasswordCredentialValue :: Maybe Text
  
instance Arbitrary Application where
  arbitrary = sized genApplication

genApplication :: Int -> Gen Application
genApplication n =
  Application
    <$> arbitraryReducedMaybe n -- applicationEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- applicationLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- applicationAccessibility :: Maybe ApplicationAccessibility
    <*> arbitraryReducedMaybe n -- applicationCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- applicationCredentials :: Maybe ApplicationCredentials
    <*> arbitraryReducedMaybe n -- applicationFeatures :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- applicationId :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- applicationLicensing :: Maybe ApplicationLicensing
    <*> arbitraryReducedMaybe n -- applicationName :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationProfile :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- applicationSettings :: Maybe ApplicationSettings
    <*> arbitraryReducedMaybe n -- applicationSignOnMode :: Maybe ApplicationSignOnMode
    <*> arbitraryReducedMaybe n -- applicationStatus :: Maybe E'Status
    <*> arbitraryReducedMaybe n -- applicationVisibility :: Maybe ApplicationVisibility
  
instance Arbitrary ApplicationAccessibility where
  arbitrary = sized genApplicationAccessibility

genApplicationAccessibility :: Int -> Gen ApplicationAccessibility
genApplicationAccessibility n =
  ApplicationAccessibility
    <$> arbitraryReducedMaybe n -- applicationAccessibilityErrorRedirectUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationAccessibilityLoginRedirectUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationAccessibilitySelfService :: Maybe Bool
  
instance Arbitrary ApplicationCredentials where
  arbitrary = sized genApplicationCredentials

genApplicationCredentials :: Int -> Gen ApplicationCredentials
genApplicationCredentials n =
  ApplicationCredentials
    <$> arbitraryReducedMaybe n -- applicationCredentialsSigning :: Maybe ApplicationCredentialsSigning
    <*> arbitraryReducedMaybe n -- applicationCredentialsUserNameTemplate :: Maybe ApplicationCredentialsUsernameTemplate
  
instance Arbitrary ApplicationCredentialsOAuthClient where
  arbitrary = sized genApplicationCredentialsOAuthClient

genApplicationCredentialsOAuthClient :: Int -> Gen ApplicationCredentialsOAuthClient
genApplicationCredentialsOAuthClient n =
  ApplicationCredentialsOAuthClient
    <$> arbitraryReducedMaybe n -- applicationCredentialsOAuthClientAutoKeyRotation :: Maybe Bool
    <*> arbitraryReducedMaybe n -- applicationCredentialsOAuthClientClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationCredentialsOAuthClientClientSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationCredentialsOAuthClientPkceRequired :: Maybe Bool
    <*> arbitraryReducedMaybe n -- applicationCredentialsOAuthClientTokenEndpointAuthMethod :: Maybe OAuthEndpointAuthenticationMethod
  
instance Arbitrary ApplicationCredentialsSigning where
  arbitrary = sized genApplicationCredentialsSigning

genApplicationCredentialsSigning :: Int -> Gen ApplicationCredentialsSigning
genApplicationCredentialsSigning n =
  ApplicationCredentialsSigning
    <$> arbitraryReducedMaybe n -- applicationCredentialsSigningKid :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationCredentialsSigningLastRotated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- applicationCredentialsSigningNextRotation :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- applicationCredentialsSigningRotationMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationCredentialsSigningUse :: Maybe ApplicationCredentialsSigningUse
  
instance Arbitrary ApplicationCredentialsUsernameTemplate where
  arbitrary = sized genApplicationCredentialsUsernameTemplate

genApplicationCredentialsUsernameTemplate :: Int -> Gen ApplicationCredentialsUsernameTemplate
genApplicationCredentialsUsernameTemplate n =
  ApplicationCredentialsUsernameTemplate
    <$> arbitraryReducedMaybe n -- applicationCredentialsUsernameTemplateSuffix :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationCredentialsUsernameTemplateTemplate :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationCredentialsUsernameTemplateType :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationCredentialsUsernameTemplatePushStatus :: Maybe Text
  
instance Arbitrary ApplicationFeature where
  arbitrary = sized genApplicationFeature

genApplicationFeature :: Int -> Gen ApplicationFeature
genApplicationFeature n =
  ApplicationFeature
    <$> arbitraryReducedMaybe n -- applicationFeatureCapabilities :: Maybe CapabilitiesObject
    <*> arbitraryReducedMaybe n -- applicationFeatureDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationFeatureLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- applicationFeatureName :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationFeatureStatus :: Maybe EnabledStatus
  
instance Arbitrary ApplicationGroupAssignment where
  arbitrary = sized genApplicationGroupAssignment

genApplicationGroupAssignment :: Int -> Gen ApplicationGroupAssignment
genApplicationGroupAssignment n =
  ApplicationGroupAssignment
    <$> arbitraryReducedMaybe n -- applicationGroupAssignmentEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- applicationGroupAssignmentLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- applicationGroupAssignmentId :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationGroupAssignmentLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- applicationGroupAssignmentPriority :: Maybe Int
    <*> arbitraryReducedMaybe n -- applicationGroupAssignmentProfile :: Maybe (Map.Map String A.Value)
  
instance Arbitrary ApplicationLicensing where
  arbitrary = sized genApplicationLicensing

genApplicationLicensing :: Int -> Gen ApplicationLicensing
genApplicationLicensing n =
  ApplicationLicensing
    <$> arbitraryReducedMaybe n -- applicationLicensingSeatCount :: Maybe Int
  
instance Arbitrary ApplicationSettings where
  arbitrary = sized genApplicationSettings

genApplicationSettings :: Int -> Gen ApplicationSettings
genApplicationSettings n =
  ApplicationSettings
    <$> arbitraryReducedMaybe n -- applicationSettingsApp :: Maybe ApplicationSettingsApplication
    <*> arbitraryReducedMaybe n -- applicationSettingsImplicitAssignment :: Maybe Bool
    <*> arbitraryReducedMaybe n -- applicationSettingsInlineHookId :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationSettingsNotifications :: Maybe ApplicationSettingsNotifications
    <*> arbitraryReducedMaybe n -- applicationSettingsNotes :: Maybe ApplicationSettingsNotes
  
instance Arbitrary ApplicationSettingsApplication where
  arbitrary = sized genApplicationSettingsApplication

genApplicationSettingsApplication :: Int -> Gen ApplicationSettingsApplication
genApplicationSettingsApplication n =
  ApplicationSettingsApplication
    <$> arbitraryReducedMaybe n -- applicationSettingsApplicationOrgName :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationSettingsApplicationUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationSettingsApplicationAcsUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationSettingsApplicationButtonField :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationSettingsApplicationPasswordField :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationSettingsApplicationUsernameField :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationSettingsApplicationLoginUrlRegex :: Maybe Text
  
instance Arbitrary ApplicationSettingsNotes where
  arbitrary = sized genApplicationSettingsNotes

genApplicationSettingsNotes :: Int -> Gen ApplicationSettingsNotes
genApplicationSettingsNotes n =
  ApplicationSettingsNotes
    <$> arbitraryReducedMaybe n -- applicationSettingsNotesAdmin :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationSettingsNotesEnduser :: Maybe Text
  
instance Arbitrary ApplicationSettingsNotifications where
  arbitrary = sized genApplicationSettingsNotifications

genApplicationSettingsNotifications :: Int -> Gen ApplicationSettingsNotifications
genApplicationSettingsNotifications n =
  ApplicationSettingsNotifications
    <$> arbitraryReducedMaybe n -- applicationSettingsNotificationsVpn :: Maybe ApplicationSettingsNotificationsVpn
  
instance Arbitrary ApplicationSettingsNotificationsVpn where
  arbitrary = sized genApplicationSettingsNotificationsVpn

genApplicationSettingsNotificationsVpn :: Int -> Gen ApplicationSettingsNotificationsVpn
genApplicationSettingsNotificationsVpn n =
  ApplicationSettingsNotificationsVpn
    <$> arbitraryReducedMaybe n -- applicationSettingsNotificationsVpnHelpUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationSettingsNotificationsVpnMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationSettingsNotificationsVpnNetwork :: Maybe ApplicationSettingsNotificationsVpnNetwork
  
instance Arbitrary ApplicationSettingsNotificationsVpnNetwork where
  arbitrary = sized genApplicationSettingsNotificationsVpnNetwork

genApplicationSettingsNotificationsVpnNetwork :: Int -> Gen ApplicationSettingsNotificationsVpnNetwork
genApplicationSettingsNotificationsVpnNetwork n =
  ApplicationSettingsNotificationsVpnNetwork
    <$> arbitraryReducedMaybe n -- applicationSettingsNotificationsVpnNetworkConnection :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationSettingsNotificationsVpnNetworkExclude :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- applicationSettingsNotificationsVpnNetworkInclude :: Maybe [Text]
  
instance Arbitrary ApplicationVisibility where
  arbitrary = sized genApplicationVisibility

genApplicationVisibility :: Int -> Gen ApplicationVisibility
genApplicationVisibility n =
  ApplicationVisibility
    <$> arbitraryReducedMaybe n -- applicationVisibilityAutoLaunch :: Maybe Bool
    <*> arbitraryReducedMaybe n -- applicationVisibilityAppLinks :: Maybe (Map.Map String Bool)
    <*> arbitraryReducedMaybe n -- applicationVisibilityAutoSubmitToolbar :: Maybe Bool
    <*> arbitraryReducedMaybe n -- applicationVisibilityHide :: Maybe ApplicationVisibilityHide
  
instance Arbitrary ApplicationVisibilityHide where
  arbitrary = sized genApplicationVisibilityHide

genApplicationVisibilityHide :: Int -> Gen ApplicationVisibilityHide
genApplicationVisibilityHide n =
  ApplicationVisibilityHide
    <$> arbitraryReducedMaybe n -- applicationVisibilityHideIOs :: Maybe Bool
    <*> arbitraryReducedMaybe n -- applicationVisibilityHideWeb :: Maybe Bool
  
instance Arbitrary AssignRoleRequest where
  arbitrary = sized genAssignRoleRequest

genAssignRoleRequest :: Int -> Gen AssignRoleRequest
genAssignRoleRequest n =
  AssignRoleRequest
    <$> arbitraryReducedMaybe n -- assignRoleRequestType :: Maybe RoleType
  
instance Arbitrary AuthenticationProvider where
  arbitrary = sized genAuthenticationProvider

genAuthenticationProvider :: Int -> Gen AuthenticationProvider
genAuthenticationProvider n =
  AuthenticationProvider
    <$> arbitraryReducedMaybe n -- authenticationProviderName :: Maybe Text
    <*> arbitraryReducedMaybe n -- authenticationProviderType :: Maybe AuthenticationProviderType
  
instance Arbitrary Authenticator where
  arbitrary = sized genAuthenticator

genAuthenticator :: Int -> Gen Authenticator
genAuthenticator n =
  Authenticator
    <$> arbitraryReducedMaybe n -- authenticatorLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- authenticatorCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- authenticatorId :: Maybe Text
    <*> arbitraryReducedMaybe n -- authenticatorKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- authenticatorStatus :: Maybe AuthenticatorStatus
    <*> arbitraryReducedMaybe n -- authenticatorLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- authenticatorName :: Maybe Text
    <*> arbitraryReducedMaybe n -- authenticatorType :: Maybe AuthenticatorType
    <*> arbitraryReducedMaybe n -- authenticatorSettings :: Maybe AuthenticatorSettings
    <*> arbitraryReducedMaybe n -- authenticatorProvider :: Maybe AuthenticatorProvider
  
instance Arbitrary AuthenticatorProvider where
  arbitrary = sized genAuthenticatorProvider

genAuthenticatorProvider :: Int -> Gen AuthenticatorProvider
genAuthenticatorProvider n =
  AuthenticatorProvider
    <$> arbitraryReducedMaybe n -- authenticatorProviderType :: Maybe Text
    <*> arbitraryReducedMaybe n -- authenticatorProviderConfiguration :: Maybe AuthenticatorProviderConfiguration
  
instance Arbitrary AuthenticatorProviderConfiguration where
  arbitrary = sized genAuthenticatorProviderConfiguration

genAuthenticatorProviderConfiguration :: Int -> Gen AuthenticatorProviderConfiguration
genAuthenticatorProviderConfiguration n =
  AuthenticatorProviderConfiguration
    <$> arbitraryReducedMaybe n -- authenticatorProviderConfigurationHostName :: Maybe Text
    <*> arbitraryReducedMaybe n -- authenticatorProviderConfigurationAuthPort :: Maybe Int
    <*> arbitraryReducedMaybe n -- authenticatorProviderConfigurationInstanceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- authenticatorProviderConfigurationSharedSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- authenticatorProviderConfigurationUserNameTemplate :: Maybe AuthenticatorProviderConfigurationUserNamePlate
    <*> arbitraryReducedMaybe n -- authenticatorProviderConfigurationHost :: Maybe Text
    <*> arbitraryReducedMaybe n -- authenticatorProviderConfigurationSecretKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- authenticatorProviderConfigurationIntegrationKey :: Maybe Text
  
instance Arbitrary AuthenticatorProviderConfigurationUserNamePlate where
  arbitrary = sized genAuthenticatorProviderConfigurationUserNamePlate

genAuthenticatorProviderConfigurationUserNamePlate :: Int -> Gen AuthenticatorProviderConfigurationUserNamePlate
genAuthenticatorProviderConfigurationUserNamePlate n =
  AuthenticatorProviderConfigurationUserNamePlate
    <$> arbitraryReducedMaybe n -- authenticatorProviderConfigurationUserNamePlateTemplate :: Maybe Text
  
instance Arbitrary AuthenticatorSettings where
  arbitrary = sized genAuthenticatorSettings

genAuthenticatorSettings :: Int -> Gen AuthenticatorSettings
genAuthenticatorSettings n =
  AuthenticatorSettings
    <$> arbitraryReducedMaybe n -- authenticatorSettingsAllowedFor :: Maybe AllowedForEnum
    <*> arbitraryReducedMaybe n -- authenticatorSettingsTokenLifetimeInMinutes :: Maybe Int
    <*> arbitraryReducedMaybe n -- authenticatorSettingsAppInstanceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- authenticatorSettingsChannelBinding :: Maybe ChannelBinding
    <*> arbitraryReducedMaybe n -- authenticatorSettingsCompliance :: Maybe Compliance
    <*> arbitraryReducedMaybe n -- authenticatorSettingsUserVerification :: Maybe UserVerificationEnum
  
instance Arbitrary AuthorizationServer where
  arbitrary = sized genAuthorizationServer

genAuthorizationServer :: Int -> Gen AuthorizationServer
genAuthorizationServer n =
  AuthorizationServer
    <$> arbitraryReducedMaybe n -- authorizationServerLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- authorizationServerAudiences :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- authorizationServerCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- authorizationServerCredentials :: Maybe AuthorizationServerCredentials
    <*> arbitraryReducedMaybe n -- authorizationServerDefault :: Maybe Bool
    <*> arbitraryReducedMaybe n -- authorizationServerDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- authorizationServerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- authorizationServerIssuer :: Maybe Text
    <*> arbitraryReducedMaybe n -- authorizationServerIssuerMode :: Maybe E'IssuerMode
    <*> arbitraryReducedMaybe n -- authorizationServerLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- authorizationServerName :: Maybe Text
    <*> arbitraryReducedMaybe n -- authorizationServerStatus :: Maybe E'Status2
  
instance Arbitrary AuthorizationServerCredentials where
  arbitrary = sized genAuthorizationServerCredentials

genAuthorizationServerCredentials :: Int -> Gen AuthorizationServerCredentials
genAuthorizationServerCredentials n =
  AuthorizationServerCredentials
    <$> arbitraryReducedMaybe n -- authorizationServerCredentialsSigning :: Maybe AuthorizationServerCredentialsSigningConfig
  
instance Arbitrary AuthorizationServerCredentialsSigningConfig where
  arbitrary = sized genAuthorizationServerCredentialsSigningConfig

genAuthorizationServerCredentialsSigningConfig :: Int -> Gen AuthorizationServerCredentialsSigningConfig
genAuthorizationServerCredentialsSigningConfig n =
  AuthorizationServerCredentialsSigningConfig
    <$> arbitraryReducedMaybe n -- authorizationServerCredentialsSigningConfigKid :: Maybe Text
    <*> arbitraryReducedMaybe n -- authorizationServerCredentialsSigningConfigLastRotated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- authorizationServerCredentialsSigningConfigNextRotation :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- authorizationServerCredentialsSigningConfigRotationMode :: Maybe AuthorizationServerCredentialsRotationMode
    <*> arbitraryReducedMaybe n -- authorizationServerCredentialsSigningConfigUse :: Maybe AuthorizationServerCredentialsUse
  
instance Arbitrary AuthorizationServerPolicy where
  arbitrary = sized genAuthorizationServerPolicy

genAuthorizationServerPolicy :: Int -> Gen AuthorizationServerPolicy
genAuthorizationServerPolicy n =
  AuthorizationServerPolicy
    <$> arbitraryReducedMaybe n -- authorizationServerPolicyEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyConditions :: Maybe PolicyRuleConditions
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyId :: Maybe Text
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyPriority :: Maybe Int
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyStatus :: Maybe E'Status2
    <*> arbitraryReducedMaybe n -- authorizationServerPolicySystem :: Maybe Bool
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyType :: Maybe PolicyType
  
instance Arbitrary AuthorizationServerPolicyRule where
  arbitrary = sized genAuthorizationServerPolicyRule

genAuthorizationServerPolicyRule :: Int -> Gen AuthorizationServerPolicyRule
genAuthorizationServerPolicyRule n =
  AuthorizationServerPolicyRule
    <$> arbitraryReducedMaybe n -- authorizationServerPolicyRuleCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyRuleId :: Maybe Text
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyRuleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyRuleLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyRulePriority :: Maybe Int
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyRuleStatus :: Maybe E'Status2
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyRuleSystem :: Maybe Bool
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyRuleType :: Maybe E'Type5
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyRuleConditions :: Maybe AuthorizationServerPolicyRuleConditions
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyRuleActions :: Maybe AuthorizationServerPolicyRuleActions
  
instance Arbitrary AuthorizationServerPolicyRuleActions where
  arbitrary = sized genAuthorizationServerPolicyRuleActions

genAuthorizationServerPolicyRuleActions :: Int -> Gen AuthorizationServerPolicyRuleActions
genAuthorizationServerPolicyRuleActions n =
  AuthorizationServerPolicyRuleActions
    <$> arbitraryReducedMaybe n -- authorizationServerPolicyRuleActionsToken :: Maybe TokenAuthorizationServerPolicyRuleAction
  
instance Arbitrary AuthorizationServerPolicyRuleConditions where
  arbitrary = sized genAuthorizationServerPolicyRuleConditions

genAuthorizationServerPolicyRuleConditions :: Int -> Gen AuthorizationServerPolicyRuleConditions
genAuthorizationServerPolicyRuleConditions n =
  AuthorizationServerPolicyRuleConditions
    <$> arbitraryReducedMaybe n -- authorizationServerPolicyRuleConditionsPeople :: Maybe PolicyPeopleCondition
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyRuleConditionsClients :: Maybe ClientPolicyCondition
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyRuleConditionsGrantTypes :: Maybe GrantTypePolicyRuleCondition
    <*> arbitraryReducedMaybe n -- authorizationServerPolicyRuleConditionsScopes :: Maybe OAuth2ScopesMediationPolicyRuleCondition
  
instance Arbitrary AutoLoginApplication where
  arbitrary = sized genAutoLoginApplication

genAutoLoginApplication :: Int -> Gen AutoLoginApplication
genAutoLoginApplication n =
  AutoLoginApplication
    <$> arbitraryReducedMaybe n -- autoLoginApplicationCredentials :: Maybe SchemeApplicationCredentials
    <*> arbitraryReducedMaybe n -- autoLoginApplicationSettings :: Maybe AutoLoginApplicationSettings
  
instance Arbitrary AutoLoginApplicationSettings where
  arbitrary = sized genAutoLoginApplicationSettings

genAutoLoginApplicationSettings :: Int -> Gen AutoLoginApplicationSettings
genAutoLoginApplicationSettings n =
  AutoLoginApplicationSettings
    <$> arbitraryReducedMaybe n -- autoLoginApplicationSettingsSignOn :: Maybe AutoLoginApplicationSettingsSignOn
  
instance Arbitrary AutoLoginApplicationSettingsSignOn where
  arbitrary = sized genAutoLoginApplicationSettingsSignOn

genAutoLoginApplicationSettingsSignOn :: Int -> Gen AutoLoginApplicationSettingsSignOn
genAutoLoginApplicationSettingsSignOn n =
  AutoLoginApplicationSettingsSignOn
    <$> arbitraryReducedMaybe n -- autoLoginApplicationSettingsSignOnLoginUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- autoLoginApplicationSettingsSignOnRedirectUrl :: Maybe Text
  
instance Arbitrary BasicApplicationSettings where
  arbitrary = sized genBasicApplicationSettings

genBasicApplicationSettings :: Int -> Gen BasicApplicationSettings
genBasicApplicationSettings n =
  BasicApplicationSettings
    <$> arbitraryReducedMaybe n -- basicApplicationSettingsApp :: Maybe BasicApplicationSettingsApplication
  
instance Arbitrary BasicApplicationSettingsApplication where
  arbitrary = sized genBasicApplicationSettingsApplication

genBasicApplicationSettingsApplication :: Int -> Gen BasicApplicationSettingsApplication
genBasicApplicationSettingsApplication n =
  BasicApplicationSettingsApplication
    <$> arbitraryReducedMaybe n -- basicApplicationSettingsApplicationAuthUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- basicApplicationSettingsApplicationUrl :: Maybe Text
  
instance Arbitrary BasicAuthApplication where
  arbitrary = sized genBasicAuthApplication

genBasicAuthApplication :: Int -> Gen BasicAuthApplication
genBasicAuthApplication n =
  BasicAuthApplication
    <$> arbitraryReducedMaybe n -- basicAuthApplicationCredentials :: Maybe SchemeApplicationCredentials
    <*> arbitraryReducedMaybeValue n -- basicAuthApplicationName :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- basicAuthApplicationSettings :: Maybe BasicApplicationSettings
  
instance Arbitrary BeforeScheduledActionPolicyRuleCondition where
  arbitrary = sized genBeforeScheduledActionPolicyRuleCondition

genBeforeScheduledActionPolicyRuleCondition :: Int -> Gen BeforeScheduledActionPolicyRuleCondition
genBeforeScheduledActionPolicyRuleCondition n =
  BeforeScheduledActionPolicyRuleCondition
    <$> arbitraryReducedMaybe n -- beforeScheduledActionPolicyRuleConditionDuration :: Maybe Duration
    <*> arbitraryReducedMaybe n -- beforeScheduledActionPolicyRuleConditionLifecycleAction :: Maybe ScheduledUserLifecycleAction
  
instance Arbitrary BookmarkApplication where
  arbitrary = sized genBookmarkApplication

genBookmarkApplication :: Int -> Gen BookmarkApplication
genBookmarkApplication n =
  BookmarkApplication
    <$> arbitraryReducedMaybeValue n -- bookmarkApplicationName :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- bookmarkApplicationSettings :: Maybe BookmarkApplicationSettings
  
instance Arbitrary BookmarkApplicationSettings where
  arbitrary = sized genBookmarkApplicationSettings

genBookmarkApplicationSettings :: Int -> Gen BookmarkApplicationSettings
genBookmarkApplicationSettings n =
  BookmarkApplicationSettings
    <$> arbitraryReducedMaybe n -- bookmarkApplicationSettingsApp :: Maybe BookmarkApplicationSettingsApplication
  
instance Arbitrary BookmarkApplicationSettingsApplication where
  arbitrary = sized genBookmarkApplicationSettingsApplication

genBookmarkApplicationSettingsApplication :: Int -> Gen BookmarkApplicationSettingsApplication
genBookmarkApplicationSettingsApplication n =
  BookmarkApplicationSettingsApplication
    <$> arbitraryReducedMaybe n -- bookmarkApplicationSettingsApplicationRequestIntegration :: Maybe Bool
    <*> arbitraryReducedMaybe n -- bookmarkApplicationSettingsApplicationUrl :: Maybe Text
  
instance Arbitrary Brand where
  arbitrary = sized genBrand

genBrand :: Int -> Gen Brand
genBrand n =
  Brand
    <$> arbitraryReducedMaybe n -- brandId :: Maybe Text
    <*> arbitraryReducedMaybe n -- brandAgreeToCustomPrivacyPolicy :: Maybe Bool
    <*> arbitraryReducedMaybe n -- brandCustomPrivacyPolicyUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- brandRemovePoweredByOkta :: Maybe Bool
    <*> arbitraryReducedMaybe n -- brandLinks :: Maybe (Map.Map String A.Value)
  
instance Arbitrary BrowserPluginApplication where
  arbitrary = sized genBrowserPluginApplication

genBrowserPluginApplication :: Int -> Gen BrowserPluginApplication
genBrowserPluginApplication n =
  BrowserPluginApplication
    <$> arbitraryReducedMaybe n -- browserPluginApplicationCredentials :: Maybe SchemeApplicationCredentials
  
instance Arbitrary CallUserFactor where
  arbitrary = sized genCallUserFactor

genCallUserFactor :: Int -> Gen CallUserFactor
genCallUserFactor n =
  CallUserFactor
    <$> arbitraryReducedMaybe n -- callUserFactorProfile :: Maybe CallUserFactorProfile
  
instance Arbitrary CallUserFactorProfile where
  arbitrary = sized genCallUserFactorProfile

genCallUserFactorProfile :: Int -> Gen CallUserFactorProfile
genCallUserFactorProfile n =
  CallUserFactorProfile
    <$> arbitraryReducedMaybe n -- callUserFactorProfilePhoneExtension :: Maybe Text
    <*> arbitraryReducedMaybe n -- callUserFactorProfilePhoneNumber :: Maybe Text
  
instance Arbitrary CapabilitiesCreateObject where
  arbitrary = sized genCapabilitiesCreateObject

genCapabilitiesCreateObject :: Int -> Gen CapabilitiesCreateObject
genCapabilitiesCreateObject n =
  CapabilitiesCreateObject
    <$> arbitraryReducedMaybe n -- capabilitiesCreateObjectLifecycleCreate :: Maybe LifecycleCreateSettingObject
  
instance Arbitrary CapabilitiesObject where
  arbitrary = sized genCapabilitiesObject

genCapabilitiesObject :: Int -> Gen CapabilitiesObject
genCapabilitiesObject n =
  CapabilitiesObject
    <$> arbitraryReducedMaybe n -- capabilitiesObjectCreate :: Maybe CapabilitiesCreateObject
    <*> arbitraryReducedMaybe n -- capabilitiesObjectUpdate :: Maybe CapabilitiesUpdateObject
  
instance Arbitrary CapabilitiesUpdateObject where
  arbitrary = sized genCapabilitiesUpdateObject

genCapabilitiesUpdateObject :: Int -> Gen CapabilitiesUpdateObject
genCapabilitiesUpdateObject n =
  CapabilitiesUpdateObject
    <$> arbitraryReducedMaybe n -- capabilitiesUpdateObjectLifecycleDeactivate :: Maybe LifecycleDeactivateSettingObject
    <*> arbitraryReducedMaybe n -- capabilitiesUpdateObjectPassword :: Maybe PasswordSettingObject
    <*> arbitraryReducedMaybe n -- capabilitiesUpdateObjectProfile :: Maybe ProfileSettingObject
  
instance Arbitrary CatalogApplication where
  arbitrary = sized genCatalogApplication

genCatalogApplication :: Int -> Gen CatalogApplication
genCatalogApplication n =
  CatalogApplication
    <$> arbitraryReducedMaybe n -- catalogApplicationId :: Maybe Text
    <*> arbitraryReducedMaybe n -- catalogApplicationName :: Maybe Text
    <*> arbitraryReducedMaybe n -- catalogApplicationDisplayName :: Maybe Text
    <*> arbitraryReducedMaybe n -- catalogApplicationDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- catalogApplicationStatus :: Maybe CatalogApplicationStatus
    <*> arbitraryReducedMaybe n -- catalogApplicationLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- catalogApplicationCategory :: Maybe Text
    <*> arbitraryReducedMaybe n -- catalogApplicationVerificationStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- catalogApplicationWebsite :: Maybe Text
    <*> arbitraryReducedMaybe n -- catalogApplicationSignOnModes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- catalogApplicationFeatures :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- catalogApplicationLinks :: Maybe (Map.Map String A.Value)
  
instance Arbitrary ChangePasswordRequest where
  arbitrary = sized genChangePasswordRequest

genChangePasswordRequest :: Int -> Gen ChangePasswordRequest
genChangePasswordRequest n =
  ChangePasswordRequest
    <$> arbitraryReducedMaybe n -- changePasswordRequestNewPassword :: Maybe PasswordCredential
    <*> arbitraryReducedMaybe n -- changePasswordRequestOldPassword :: Maybe PasswordCredential
  
instance Arbitrary ChannelBinding where
  arbitrary = sized genChannelBinding

genChannelBinding :: Int -> Gen ChannelBinding
genChannelBinding n =
  ChannelBinding
    <$> arbitraryReducedMaybe n -- channelBindingStyle :: Maybe Text
    <*> arbitraryReducedMaybe n -- channelBindingRequired :: Maybe RequiredEnum
  
instance Arbitrary ClientPolicyCondition where
  arbitrary = sized genClientPolicyCondition

genClientPolicyCondition :: Int -> Gen ClientPolicyCondition
genClientPolicyCondition n =
  ClientPolicyCondition
    <$> arbitraryReducedMaybe n -- clientPolicyConditionInclude :: Maybe [Text]
  
instance Arbitrary ClientSecret where
  arbitrary = sized genClientSecret

genClientSecret :: Int -> Gen ClientSecret
genClientSecret n =
  ClientSecret
    <$> arbitraryReducedMaybe n -- clientSecretId :: Maybe Text
    <*> arbitraryReducedMaybe n -- clientSecretClientSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- clientSecretSecretHash :: Maybe Text
    <*> arbitraryReducedMaybe n -- clientSecretCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- clientSecretLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- clientSecretStatus :: Maybe E'Status2
    <*> arbitraryReducedMaybe n -- clientSecretLinks :: Maybe (Map.Map String A.Value)
  
instance Arbitrary ClientSecretMetadata where
  arbitrary = sized genClientSecretMetadata

genClientSecretMetadata :: Int -> Gen ClientSecretMetadata
genClientSecretMetadata n =
  ClientSecretMetadata
    <$> arbitraryReducedMaybe n -- clientSecretMetadataClientSecret :: Maybe Text
  
instance Arbitrary Compliance where
  arbitrary = sized genCompliance

genCompliance :: Int -> Gen Compliance
genCompliance n =
  Compliance
    <$> arbitraryReducedMaybe n -- complianceFips :: Maybe FipsEnum
  
instance Arbitrary ContextPolicyRuleCondition where
  arbitrary = sized genContextPolicyRuleCondition

genContextPolicyRuleCondition :: Int -> Gen ContextPolicyRuleCondition
genContextPolicyRuleCondition n =
  ContextPolicyRuleCondition
    <$> arbitraryReducedMaybe n -- contextPolicyRuleConditionExpression :: Maybe Text
  
instance Arbitrary CreateSessionRequest where
  arbitrary = sized genCreateSessionRequest

genCreateSessionRequest :: Int -> Gen CreateSessionRequest
genCreateSessionRequest n =
  CreateSessionRequest
    <$> arbitraryReducedMaybe n -- createSessionRequestSessionToken :: Maybe Text
  
instance Arbitrary CreateUserRequest where
  arbitrary = sized genCreateUserRequest

genCreateUserRequest :: Int -> Gen CreateUserRequest
genCreateUserRequest n =
  CreateUserRequest
    <$> arbitraryReducedMaybe n -- createUserRequestCredentials :: Maybe UserCredentials
    <*> arbitraryReducedMaybe n -- createUserRequestGroupIds :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- createUserRequestProfile :: Maybe UserProfile
    <*> arbitraryReducedMaybe n -- createUserRequestType :: Maybe UserType
  
instance Arbitrary Csr where
  arbitrary = sized genCsr

genCsr :: Int -> Gen Csr
genCsr n =
  Csr
    <$> arbitraryReducedMaybe n -- csrCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- csrCsr :: Maybe Text
    <*> arbitraryReducedMaybe n -- csrId :: Maybe Text
    <*> arbitraryReducedMaybe n -- csrKty :: Maybe Text
  
instance Arbitrary CsrMetadata where
  arbitrary = sized genCsrMetadata

genCsrMetadata :: Int -> Gen CsrMetadata
genCsrMetadata n =
  CsrMetadata
    <$> arbitraryReducedMaybe n -- csrMetadataSubject :: Maybe CsrMetadataSubject
    <*> arbitraryReducedMaybe n -- csrMetadataSubjectAltNames :: Maybe CsrMetadataSubjectAltNames
  
instance Arbitrary CsrMetadataSubject where
  arbitrary = sized genCsrMetadataSubject

genCsrMetadataSubject :: Int -> Gen CsrMetadataSubject
genCsrMetadataSubject n =
  CsrMetadataSubject
    <$> arbitraryReducedMaybe n -- csrMetadataSubjectCommonName :: Maybe Text
    <*> arbitraryReducedMaybe n -- csrMetadataSubjectCountryName :: Maybe Text
    <*> arbitraryReducedMaybe n -- csrMetadataSubjectLocalityName :: Maybe Text
    <*> arbitraryReducedMaybe n -- csrMetadataSubjectOrganizationName :: Maybe Text
    <*> arbitraryReducedMaybe n -- csrMetadataSubjectOrganizationalUnitName :: Maybe Text
    <*> arbitraryReducedMaybe n -- csrMetadataSubjectStateOrProvinceName :: Maybe Text
  
instance Arbitrary CsrMetadataSubjectAltNames where
  arbitrary = sized genCsrMetadataSubjectAltNames

genCsrMetadataSubjectAltNames :: Int -> Gen CsrMetadataSubjectAltNames
genCsrMetadataSubjectAltNames n =
  CsrMetadataSubjectAltNames
    <$> arbitraryReducedMaybe n -- csrMetadataSubjectAltNamesDnsNames :: Maybe [Text]
  
instance Arbitrary CustomHotpUserFactor where
  arbitrary = sized genCustomHotpUserFactor

genCustomHotpUserFactor :: Int -> Gen CustomHotpUserFactor
genCustomHotpUserFactor n =
  CustomHotpUserFactor
    <$> arbitraryReducedMaybe n -- customHotpUserFactorFactorProfileId :: Maybe Text
    <*> arbitraryReducedMaybe n -- customHotpUserFactorProfile :: Maybe CustomHotpUserFactorProfile
  
instance Arbitrary CustomHotpUserFactorProfile where
  arbitrary = sized genCustomHotpUserFactorProfile

genCustomHotpUserFactorProfile :: Int -> Gen CustomHotpUserFactorProfile
genCustomHotpUserFactorProfile n =
  CustomHotpUserFactorProfile
    <$> arbitraryReducedMaybe n -- customHotpUserFactorProfileSharedSecret :: Maybe Text
  
instance Arbitrary DNSRecord where
  arbitrary = sized genDNSRecord

genDNSRecord :: Int -> Gen DNSRecord
genDNSRecord n =
  DNSRecord
    <$> arbitraryReducedMaybe n -- dNSRecordExpiration :: Maybe Text
    <*> arbitraryReducedMaybe n -- dNSRecordFqdn :: Maybe Text
    <*> arbitraryReducedMaybe n -- dNSRecordRecordType :: Maybe DNSRecordType
    <*> arbitraryReducedMaybe n -- dNSRecordValues :: Maybe [Text]
  
instance Arbitrary DeviceAccessPolicyRuleCondition where
  arbitrary = sized genDeviceAccessPolicyRuleCondition

genDeviceAccessPolicyRuleCondition :: Int -> Gen DeviceAccessPolicyRuleCondition
genDeviceAccessPolicyRuleCondition n =
  DeviceAccessPolicyRuleCondition
    <$> arbitraryReducedMaybe n -- deviceAccessPolicyRuleConditionRegistered :: Maybe Bool
    <*> arbitraryReducedMaybe n -- deviceAccessPolicyRuleConditionManaged :: Maybe Bool
  
instance Arbitrary DevicePolicyRuleCondition where
  arbitrary = sized genDevicePolicyRuleCondition

genDevicePolicyRuleCondition :: Int -> Gen DevicePolicyRuleCondition
genDevicePolicyRuleCondition n =
  DevicePolicyRuleCondition
    <$> arbitraryReducedMaybe n -- devicePolicyRuleConditionMigrated :: Maybe Bool
    <*> arbitraryReducedMaybe n -- devicePolicyRuleConditionPlatform :: Maybe DevicePolicyRuleConditionPlatform
    <*> arbitraryReducedMaybe n -- devicePolicyRuleConditionRooted :: Maybe Bool
    <*> arbitraryReducedMaybe n -- devicePolicyRuleConditionTrustLevel :: Maybe E'TrustLevel
  
instance Arbitrary DevicePolicyRuleConditionPlatform where
  arbitrary = sized genDevicePolicyRuleConditionPlatform

genDevicePolicyRuleConditionPlatform :: Int -> Gen DevicePolicyRuleConditionPlatform
genDevicePolicyRuleConditionPlatform n =
  DevicePolicyRuleConditionPlatform
    <$> arbitraryReducedMaybe n -- devicePolicyRuleConditionPlatformSupportedMdmFrameworks :: Maybe [E'SupportedMdmFrameworks]
    <*> arbitraryReducedMaybe n -- devicePolicyRuleConditionPlatformTypes :: Maybe [E'Types]
  
instance Arbitrary Domain where
  arbitrary = sized genDomain

genDomain :: Int -> Gen Domain
genDomain n =
  Domain
    <$> arbitraryReducedMaybe n -- domainId :: Maybe Text
    <*> arbitraryReducedMaybe n -- domainCertificateSourceType :: Maybe DomainCertificateSourceType
    <*> arbitraryReducedMaybe n -- domainDomain :: Maybe Text
    <*> arbitraryReducedMaybe n -- domainDnsRecords :: Maybe [DNSRecord]
    <*> arbitraryReducedMaybe n -- domainPublicCertificate :: Maybe DomainCertificateMetadata
    <*> arbitraryReducedMaybe n -- domainValidationStatus :: Maybe DomainValidationStatus
  
instance Arbitrary DomainCertificate where
  arbitrary = sized genDomainCertificate

genDomainCertificate :: Int -> Gen DomainCertificate
genDomainCertificate n =
  DomainCertificate
    <$> arbitraryReducedMaybe n -- domainCertificateCertificate :: Maybe Text
    <*> arbitraryReducedMaybe n -- domainCertificateCertificateChain :: Maybe Text
    <*> arbitraryReducedMaybe n -- domainCertificatePrivateKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- domainCertificateType :: Maybe DomainCertificateType
  
instance Arbitrary DomainCertificateMetadata where
  arbitrary = sized genDomainCertificateMetadata

genDomainCertificateMetadata :: Int -> Gen DomainCertificateMetadata
genDomainCertificateMetadata n =
  DomainCertificateMetadata
    <$> arbitraryReducedMaybe n -- domainCertificateMetadataExpiration :: Maybe Text
    <*> arbitraryReducedMaybe n -- domainCertificateMetadataFingerprint :: Maybe Text
    <*> arbitraryReducedMaybe n -- domainCertificateMetadataSubject :: Maybe Text
  
instance Arbitrary DomainListResponse where
  arbitrary = sized genDomainListResponse

genDomainListResponse :: Int -> Gen DomainListResponse
genDomainListResponse n =
  DomainListResponse
    <$> arbitraryReducedMaybe n -- domainListResponseDomains :: Maybe [Domain]
  
instance Arbitrary Duration where
  arbitrary = sized genDuration

genDuration :: Int -> Gen Duration
genDuration n =
  Duration
    <$> arbitraryReducedMaybe n -- durationNumber :: Maybe Int
    <*> arbitraryReducedMaybe n -- durationUnit :: Maybe Text
  
instance Arbitrary EmailTemplate where
  arbitrary = sized genEmailTemplate

genEmailTemplate :: Int -> Gen EmailTemplate
genEmailTemplate n =
  EmailTemplate
    <$> arbitraryReducedMaybe n -- emailTemplateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- emailTemplateLinks :: Maybe (Map.Map String A.Value)
  
instance Arbitrary EmailTemplateContent where
  arbitrary = sized genEmailTemplateContent

genEmailTemplateContent :: Int -> Gen EmailTemplateContent
genEmailTemplateContent n =
  EmailTemplateContent
    <$> arbitraryReducedMaybe n -- emailTemplateContentBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- emailTemplateContentFromAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- emailTemplateContentFromName :: Maybe Text
    <*> arbitraryReducedMaybe n -- emailTemplateContentSubject :: Maybe Text
    <*> arbitraryReducedMaybe n -- emailTemplateContentLinks :: Maybe (Map.Map String A.Value)
  
instance Arbitrary EmailTemplateCustomization where
  arbitrary = sized genEmailTemplateCustomization

genEmailTemplateCustomization :: Int -> Gen EmailTemplateCustomization
genEmailTemplateCustomization n =
  EmailTemplateCustomization
    <$> arbitraryReducedMaybe n -- emailTemplateCustomizationBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- emailTemplateCustomizationLanguage :: Maybe Text
    <*> arbitraryReducedMaybe n -- emailTemplateCustomizationSubject :: Maybe Text
    <*> arbitraryReducedMaybe n -- emailTemplateCustomizationId :: Maybe Text
    <*> arbitraryReducedMaybe n -- emailTemplateCustomizationCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- emailTemplateCustomizationIsDefault :: Maybe Bool
    <*> arbitraryReducedMaybe n -- emailTemplateCustomizationLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- emailTemplateCustomizationLinks :: Maybe (Map.Map String A.Value)
  
instance Arbitrary EmailTemplateCustomizationRequest where
  arbitrary = sized genEmailTemplateCustomizationRequest

genEmailTemplateCustomizationRequest :: Int -> Gen EmailTemplateCustomizationRequest
genEmailTemplateCustomizationRequest n =
  EmailTemplateCustomizationRequest
    <$> arbitraryReducedMaybe n -- emailTemplateCustomizationRequestBody :: Maybe Text
    <*> arbitraryReducedMaybe n -- emailTemplateCustomizationRequestLanguage :: Maybe Text
    <*> arbitraryReducedMaybe n -- emailTemplateCustomizationRequestSubject :: Maybe Text
    <*> arbitraryReducedMaybe n -- emailTemplateCustomizationRequestIsDefault :: Maybe Bool
  
instance Arbitrary EmailTemplateTestRequest where
  arbitrary = sized genEmailTemplateTestRequest

genEmailTemplateTestRequest :: Int -> Gen EmailTemplateTestRequest
genEmailTemplateTestRequest n =
  EmailTemplateTestRequest
    <$> arbitraryReducedMaybe n -- emailTemplateTestRequestCustomizationId :: Maybe Text
  
instance Arbitrary EmailUserFactor where
  arbitrary = sized genEmailUserFactor

genEmailUserFactor :: Int -> Gen EmailUserFactor
genEmailUserFactor n =
  EmailUserFactor
    <$> arbitraryReducedMaybe n -- emailUserFactorProfile :: Maybe EmailUserFactorProfile
  
instance Arbitrary EmailUserFactorProfile where
  arbitrary = sized genEmailUserFactorProfile

genEmailUserFactorProfile :: Int -> Gen EmailUserFactorProfile
genEmailUserFactorProfile n =
  EmailUserFactorProfile
    <$> arbitraryReducedMaybe n -- emailUserFactorProfileEmail :: Maybe Text
  
instance Arbitrary EventHook where
  arbitrary = sized genEventHook

genEventHook :: Int -> Gen EventHook
genEventHook n =
  EventHook
    <$> arbitraryReducedMaybe n -- eventHookLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- eventHookChannel :: Maybe EventHookChannel
    <*> arbitraryReducedMaybe n -- eventHookCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- eventHookCreatedBy :: Maybe Text
    <*> arbitraryReducedMaybe n -- eventHookEvents :: Maybe EventSubscriptions
    <*> arbitraryReducedMaybe n -- eventHookId :: Maybe Text
    <*> arbitraryReducedMaybe n -- eventHookLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- eventHookName :: Maybe Text
    <*> arbitraryReducedMaybe n -- eventHookStatus :: Maybe E'Status2
    <*> arbitraryReducedMaybe n -- eventHookVerificationStatus :: Maybe E'VerificationStatus
  
instance Arbitrary EventHookChannel where
  arbitrary = sized genEventHookChannel

genEventHookChannel :: Int -> Gen EventHookChannel
genEventHookChannel n =
  EventHookChannel
    <$> arbitraryReducedMaybe n -- eventHookChannelConfig :: Maybe EventHookChannelConfig
    <*> arbitraryReducedMaybe n -- eventHookChannelType :: Maybe E'Type2
    <*> arbitraryReducedMaybe n -- eventHookChannelVersion :: Maybe Text
  
instance Arbitrary EventHookChannelConfig where
  arbitrary = sized genEventHookChannelConfig

genEventHookChannelConfig :: Int -> Gen EventHookChannelConfig
genEventHookChannelConfig n =
  EventHookChannelConfig
    <$> arbitraryReducedMaybe n -- eventHookChannelConfigAuthScheme :: Maybe EventHookChannelConfigAuthScheme
    <*> arbitraryReducedMaybe n -- eventHookChannelConfigHeaders :: Maybe [EventHookChannelConfigHeader]
    <*> arbitraryReducedMaybe n -- eventHookChannelConfigUri :: Maybe Text
  
instance Arbitrary EventHookChannelConfigAuthScheme where
  arbitrary = sized genEventHookChannelConfigAuthScheme

genEventHookChannelConfigAuthScheme :: Int -> Gen EventHookChannelConfigAuthScheme
genEventHookChannelConfigAuthScheme n =
  EventHookChannelConfigAuthScheme
    <$> arbitraryReducedMaybe n -- eventHookChannelConfigAuthSchemeKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- eventHookChannelConfigAuthSchemeType :: Maybe EventHookChannelConfigAuthSchemeType
    <*> arbitraryReducedMaybe n -- eventHookChannelConfigAuthSchemeValue :: Maybe Text
  
instance Arbitrary EventHookChannelConfigHeader where
  arbitrary = sized genEventHookChannelConfigHeader

genEventHookChannelConfigHeader :: Int -> Gen EventHookChannelConfigHeader
genEventHookChannelConfigHeader n =
  EventHookChannelConfigHeader
    <$> arbitraryReducedMaybe n -- eventHookChannelConfigHeaderKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- eventHookChannelConfigHeaderValue :: Maybe Text
  
instance Arbitrary EventSubscriptions where
  arbitrary = sized genEventSubscriptions

genEventSubscriptions :: Int -> Gen EventSubscriptions
genEventSubscriptions n =
  EventSubscriptions
    <$> arbitraryReducedMaybe n -- eventSubscriptionsItems :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- eventSubscriptionsType :: Maybe E'Type3
  
instance Arbitrary Feature where
  arbitrary = sized genFeature

genFeature :: Int -> Gen Feature
genFeature n =
  Feature
    <$> arbitraryReducedMaybe n -- featureLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- featureDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- featureId :: Maybe Text
    <*> arbitraryReducedMaybe n -- featureName :: Maybe Text
    <*> arbitraryReducedMaybe n -- featureStage :: Maybe FeatureStage
    <*> arbitraryReducedMaybe n -- featureStatus :: Maybe EnabledStatus
    <*> arbitraryReducedMaybe n -- featureType :: Maybe FeatureType
  
instance Arbitrary FeatureStage where
  arbitrary = sized genFeatureStage

genFeatureStage :: Int -> Gen FeatureStage
genFeatureStage n =
  FeatureStage
    <$> arbitraryReducedMaybe n -- featureStageState :: Maybe FeatureStageState
    <*> arbitraryReducedMaybe n -- featureStageValue :: Maybe FeatureStageValue
  
instance Arbitrary ForgotPasswordResponse where
  arbitrary = sized genForgotPasswordResponse

genForgotPasswordResponse :: Int -> Gen ForgotPasswordResponse
genForgotPasswordResponse n =
  ForgotPasswordResponse
    <$> arbitraryReducedMaybe n -- forgotPasswordResponseResetPasswordUrl :: Maybe Text
  
instance Arbitrary GrantTypePolicyRuleCondition where
  arbitrary = sized genGrantTypePolicyRuleCondition

genGrantTypePolicyRuleCondition :: Int -> Gen GrantTypePolicyRuleCondition
genGrantTypePolicyRuleCondition n =
  GrantTypePolicyRuleCondition
    <$> arbitraryReducedMaybe n -- grantTypePolicyRuleConditionInclude :: Maybe [Text]
  
instance Arbitrary Group where
  arbitrary = sized genGroup

genGroup :: Int -> Gen Group
genGroup n =
  Group
    <$> arbitraryReducedMaybe n -- groupEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- groupLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- groupCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- groupId :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupLastMembershipUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- groupLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- groupObjectClass :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- groupProfile :: Maybe GroupProfile
    <*> arbitraryReducedMaybe n -- groupType :: Maybe GroupType
  
instance Arbitrary GroupCondition where
  arbitrary = sized genGroupCondition

genGroupCondition :: Int -> Gen GroupCondition
genGroupCondition n =
  GroupCondition
    <$> arbitraryReducedMaybe n -- groupConditionExclude :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- groupConditionInclude :: Maybe [Text]
  
instance Arbitrary GroupPolicyRuleCondition where
  arbitrary = sized genGroupPolicyRuleCondition

genGroupPolicyRuleCondition :: Int -> Gen GroupPolicyRuleCondition
genGroupPolicyRuleCondition n =
  GroupPolicyRuleCondition
    <$> arbitraryReducedMaybe n -- groupPolicyRuleConditionExclude :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- groupPolicyRuleConditionInclude :: Maybe [Text]
  
instance Arbitrary GroupProfile where
  arbitrary = sized genGroupProfile

genGroupProfile :: Int -> Gen GroupProfile
genGroupProfile n =
  GroupProfile
    <$> arbitraryReducedMaybe n -- groupProfileDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupProfileName :: Maybe Text
  
instance Arbitrary GroupRule where
  arbitrary = sized genGroupRule

genGroupRule :: Int -> Gen GroupRule
genGroupRule n =
  GroupRule
    <$> arbitraryReducedMaybe n -- groupRuleActions :: Maybe GroupRuleAction
    <*> arbitraryReducedMaybe n -- groupRuleConditions :: Maybe GroupRuleConditions
    <*> arbitraryReducedMaybe n -- groupRuleCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- groupRuleId :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupRuleLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- groupRuleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupRuleStatus :: Maybe GroupRuleStatus
    <*> arbitraryReducedMaybe n -- groupRuleType :: Maybe Text
  
instance Arbitrary GroupRuleAction where
  arbitrary = sized genGroupRuleAction

genGroupRuleAction :: Int -> Gen GroupRuleAction
genGroupRuleAction n =
  GroupRuleAction
    <$> arbitraryReducedMaybe n -- groupRuleActionAssignUserToGroups :: Maybe GroupRuleGroupAssignment
  
instance Arbitrary GroupRuleConditions where
  arbitrary = sized genGroupRuleConditions

genGroupRuleConditions :: Int -> Gen GroupRuleConditions
genGroupRuleConditions n =
  GroupRuleConditions
    <$> arbitraryReducedMaybe n -- groupRuleConditionsExpression :: Maybe GroupRuleExpression
    <*> arbitraryReducedMaybe n -- groupRuleConditionsPeople :: Maybe GroupRulePeopleCondition
  
instance Arbitrary GroupRuleExpression where
  arbitrary = sized genGroupRuleExpression

genGroupRuleExpression :: Int -> Gen GroupRuleExpression
genGroupRuleExpression n =
  GroupRuleExpression
    <$> arbitraryReducedMaybe n -- groupRuleExpressionType :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupRuleExpressionValue :: Maybe Text
  
instance Arbitrary GroupRuleGroupAssignment where
  arbitrary = sized genGroupRuleGroupAssignment

genGroupRuleGroupAssignment :: Int -> Gen GroupRuleGroupAssignment
genGroupRuleGroupAssignment n =
  GroupRuleGroupAssignment
    <$> arbitraryReducedMaybe n -- groupRuleGroupAssignmentGroupIds :: Maybe [Text]
  
instance Arbitrary GroupRuleGroupCondition where
  arbitrary = sized genGroupRuleGroupCondition

genGroupRuleGroupCondition :: Int -> Gen GroupRuleGroupCondition
genGroupRuleGroupCondition n =
  GroupRuleGroupCondition
    <$> arbitraryReducedMaybe n -- groupRuleGroupConditionExclude :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- groupRuleGroupConditionInclude :: Maybe [Text]
  
instance Arbitrary GroupRulePeopleCondition where
  arbitrary = sized genGroupRulePeopleCondition

genGroupRulePeopleCondition :: Int -> Gen GroupRulePeopleCondition
genGroupRulePeopleCondition n =
  GroupRulePeopleCondition
    <$> arbitraryReducedMaybe n -- groupRulePeopleConditionGroups :: Maybe GroupRuleGroupCondition
    <*> arbitraryReducedMaybe n -- groupRulePeopleConditionUsers :: Maybe GroupRuleUserCondition
  
instance Arbitrary GroupRuleUserCondition where
  arbitrary = sized genGroupRuleUserCondition

genGroupRuleUserCondition :: Int -> Gen GroupRuleUserCondition
genGroupRuleUserCondition n =
  GroupRuleUserCondition
    <$> arbitraryReducedMaybe n -- groupRuleUserConditionExclude :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- groupRuleUserConditionInclude :: Maybe [Text]
  
instance Arbitrary GroupSchema where
  arbitrary = sized genGroupSchema

genGroupSchema :: Int -> Gen GroupSchema
genGroupSchema n =
  GroupSchema
    <$> arbitraryReducedMaybe n -- groupSchemaId :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupSchemaSchema :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupSchemaName :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupSchemaTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupSchemaLastUpdated :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupSchemaDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupSchemaCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupSchemaDefinitions :: Maybe GroupSchemaDefinitions
    <*> arbitraryReducedMaybe n -- groupSchemaType :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupSchemaProperties :: Maybe UserSchemaProperties
    <*> arbitraryReducedMaybe n -- groupSchemaLinks :: Maybe (Map.Map String A.Value)
  
instance Arbitrary GroupSchemaAttribute where
  arbitrary = sized genGroupSchemaAttribute

genGroupSchemaAttribute :: Int -> Gen GroupSchemaAttribute
genGroupSchemaAttribute n =
  GroupSchemaAttribute
    <$> arbitraryReducedMaybe n -- groupSchemaAttributeTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupSchemaAttributeType :: Maybe UserSchemaAttributeType
    <*> arbitraryReducedMaybe n -- groupSchemaAttributeRequired :: Maybe Bool
    <*> arbitraryReducedMaybe n -- groupSchemaAttributeMutability :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupSchemaAttributeScope :: Maybe UserSchemaAttributeScope
    <*> arbitraryReducedMaybe n -- groupSchemaAttributeEnum :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- groupSchemaAttributeOneOf :: Maybe [UserSchemaAttributeEnum]
    <*> arbitraryReducedMaybe n -- groupSchemaAttributeMinLength :: Maybe Int
    <*> arbitraryReducedMaybe n -- groupSchemaAttributeMaxLength :: Maybe Int
    <*> arbitraryReducedMaybe n -- groupSchemaAttributeDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupSchemaAttributePermissions :: Maybe [UserSchemaAttributePermission]
    <*> arbitraryReducedMaybe n -- groupSchemaAttributeMaster :: Maybe UserSchemaAttributeMaster
    <*> arbitraryReducedMaybe n -- groupSchemaAttributeUnion :: Maybe UserSchemaAttributeUnion
    <*> arbitraryReducedMaybe n -- groupSchemaAttributeItems :: Maybe UserSchemaAttributeItems
    <*> arbitraryReducedMaybe n -- groupSchemaAttributeUnique :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupSchemaAttributeExternalName :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupSchemaAttributeExternalNamespace :: Maybe Text
  
instance Arbitrary GroupSchemaBase where
  arbitrary = sized genGroupSchemaBase

genGroupSchemaBase :: Int -> Gen GroupSchemaBase
genGroupSchemaBase n =
  GroupSchemaBase
    <$> arbitraryReducedMaybe n -- groupSchemaBaseId :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupSchemaBaseType :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupSchemaBaseProperties :: Maybe GroupSchemaBaseProperties
    <*> arbitraryReducedMaybe n -- groupSchemaBaseRequired :: Maybe [Text]
  
instance Arbitrary GroupSchemaBaseProperties where
  arbitrary = sized genGroupSchemaBaseProperties

genGroupSchemaBaseProperties :: Int -> Gen GroupSchemaBaseProperties
genGroupSchemaBaseProperties n =
  GroupSchemaBaseProperties
    <$> arbitraryReducedMaybe n -- groupSchemaBasePropertiesName :: Maybe GroupSchemaAttribute
    <*> arbitraryReducedMaybe n -- groupSchemaBasePropertiesDescription :: Maybe GroupSchemaAttribute
  
instance Arbitrary GroupSchemaCustom where
  arbitrary = sized genGroupSchemaCustom

genGroupSchemaCustom :: Int -> Gen GroupSchemaCustom
genGroupSchemaCustom n =
  GroupSchemaCustom
    <$> arbitraryReducedMaybe n -- groupSchemaCustomId :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupSchemaCustomType :: Maybe Text
    <*> arbitraryReducedMaybe n -- groupSchemaCustomProperties :: Maybe (Map.Map String GroupSchemaAttribute)
    <*> arbitraryReducedMaybe n -- groupSchemaCustomRequired :: Maybe [Text]
  
instance Arbitrary GroupSchemaDefinitions where
  arbitrary = sized genGroupSchemaDefinitions

genGroupSchemaDefinitions :: Int -> Gen GroupSchemaDefinitions
genGroupSchemaDefinitions n =
  GroupSchemaDefinitions
    <$> arbitraryReducedMaybe n -- groupSchemaDefinitionsBase :: Maybe GroupSchemaBase
    <*> arbitraryReducedMaybe n -- groupSchemaDefinitionsCustom :: Maybe GroupSchemaCustom
  
instance Arbitrary HardwareUserFactor where
  arbitrary = sized genHardwareUserFactor

genHardwareUserFactor :: Int -> Gen HardwareUserFactor
genHardwareUserFactor n =
  HardwareUserFactor
    <$> arbitraryReducedMaybe n -- hardwareUserFactorProfile :: Maybe HardwareUserFactorProfile
  
instance Arbitrary HardwareUserFactorProfile where
  arbitrary = sized genHardwareUserFactorProfile

genHardwareUserFactorProfile :: Int -> Gen HardwareUserFactorProfile
genHardwareUserFactorProfile n =
  HardwareUserFactorProfile
    <$> arbitraryReducedMaybe n -- hardwareUserFactorProfileCredentialId :: Maybe Text
  
instance Arbitrary IdentityProvider where
  arbitrary = sized genIdentityProvider

genIdentityProvider :: Int -> Gen IdentityProvider
genIdentityProvider n =
  IdentityProvider
    <$> arbitraryReducedMaybe n -- identityProviderLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- identityProviderCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- identityProviderId :: Maybe Text
    <*> arbitraryReducedMaybe n -- identityProviderIssuerMode :: Maybe E'IssuerMode
    <*> arbitraryReducedMaybe n -- identityProviderLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- identityProviderName :: Maybe Text
    <*> arbitraryReducedMaybe n -- identityProviderPolicy :: Maybe IdentityProviderPolicy
    <*> arbitraryReducedMaybe n -- identityProviderProtocol :: Maybe Protocol
    <*> arbitraryReducedMaybe n -- identityProviderStatus :: Maybe E'Status2
    <*> arbitraryReducedMaybe n -- identityProviderType :: Maybe E'Type4
  
instance Arbitrary IdentityProviderApplicationUser where
  arbitrary = sized genIdentityProviderApplicationUser

genIdentityProviderApplicationUser :: Int -> Gen IdentityProviderApplicationUser
genIdentityProviderApplicationUser n =
  IdentityProviderApplicationUser
    <$> arbitraryReducedMaybe n -- identityProviderApplicationUserEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- identityProviderApplicationUserLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- identityProviderApplicationUserCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- identityProviderApplicationUserExternalId :: Maybe Text
    <*> arbitraryReducedMaybe n -- identityProviderApplicationUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- identityProviderApplicationUserLastUpdated :: Maybe Text
    <*> arbitraryReducedMaybe n -- identityProviderApplicationUserProfile :: Maybe (Map.Map String A.Value)
  
instance Arbitrary IdentityProviderCredentials where
  arbitrary = sized genIdentityProviderCredentials

genIdentityProviderCredentials :: Int -> Gen IdentityProviderCredentials
genIdentityProviderCredentials n =
  IdentityProviderCredentials
    <$> arbitraryReducedMaybe n -- identityProviderCredentialsClient :: Maybe IdentityProviderCredentialsClient
    <*> arbitraryReducedMaybe n -- identityProviderCredentialsSigning :: Maybe IdentityProviderCredentialsSigning
    <*> arbitraryReducedMaybe n -- identityProviderCredentialsTrust :: Maybe IdentityProviderCredentialsTrust
  
instance Arbitrary IdentityProviderCredentialsClient where
  arbitrary = sized genIdentityProviderCredentialsClient

genIdentityProviderCredentialsClient :: Int -> Gen IdentityProviderCredentialsClient
genIdentityProviderCredentialsClient n =
  IdentityProviderCredentialsClient
    <$> arbitraryReducedMaybe n -- identityProviderCredentialsClientClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- identityProviderCredentialsClientClientSecret :: Maybe Text
  
instance Arbitrary IdentityProviderCredentialsSigning where
  arbitrary = sized genIdentityProviderCredentialsSigning

genIdentityProviderCredentialsSigning :: Int -> Gen IdentityProviderCredentialsSigning
genIdentityProviderCredentialsSigning n =
  IdentityProviderCredentialsSigning
    <$> arbitraryReducedMaybe n -- identityProviderCredentialsSigningKid :: Maybe Text
    <*> arbitraryReducedMaybe n -- identityProviderCredentialsSigningPrivateKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- identityProviderCredentialsSigningTeamId :: Maybe Text
  
instance Arbitrary IdentityProviderCredentialsTrust where
  arbitrary = sized genIdentityProviderCredentialsTrust

genIdentityProviderCredentialsTrust :: Int -> Gen IdentityProviderCredentialsTrust
genIdentityProviderCredentialsTrust n =
  IdentityProviderCredentialsTrust
    <$> arbitraryReducedMaybe n -- identityProviderCredentialsTrustAudience :: Maybe Text
    <*> arbitraryReducedMaybe n -- identityProviderCredentialsTrustIssuer :: Maybe Text
    <*> arbitraryReducedMaybe n -- identityProviderCredentialsTrustKid :: Maybe Text
    <*> arbitraryReducedMaybe n -- identityProviderCredentialsTrustRevocation :: Maybe E'Revocation
    <*> arbitraryReducedMaybe n -- identityProviderCredentialsTrustRevocationCacheLifetime :: Maybe Int
  
instance Arbitrary IdentityProviderPolicy where
  arbitrary = sized genIdentityProviderPolicy

genIdentityProviderPolicy :: Int -> Gen IdentityProviderPolicy
genIdentityProviderPolicy n =
  IdentityProviderPolicy
    <$> arbitraryReducedMaybe n -- identityProviderPolicyAccountLink :: Maybe PolicyAccountLink
    <*> arbitraryReducedMaybe n -- identityProviderPolicyMaxClockSkew :: Maybe Int
    <*> arbitraryReducedMaybe n -- identityProviderPolicyProvisioning :: Maybe Provisioning
    <*> arbitraryReducedMaybe n -- identityProviderPolicySubject :: Maybe PolicySubject
  
instance Arbitrary IdentityProviderPolicyRuleCondition where
  arbitrary = sized genIdentityProviderPolicyRuleCondition

genIdentityProviderPolicyRuleCondition :: Int -> Gen IdentityProviderPolicyRuleCondition
genIdentityProviderPolicyRuleCondition n =
  IdentityProviderPolicyRuleCondition
    <$> arbitraryReducedMaybe n -- identityProviderPolicyRuleConditionIdpIds :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- identityProviderPolicyRuleConditionProvider :: Maybe E'Provider
  
instance Arbitrary IdpPolicyRuleAction where
  arbitrary = sized genIdpPolicyRuleAction

genIdpPolicyRuleAction :: Int -> Gen IdpPolicyRuleAction
genIdpPolicyRuleAction n =
  IdpPolicyRuleAction
    <$> arbitraryReducedMaybe n -- idpPolicyRuleActionProviders :: Maybe [IdpPolicyRuleActionProvider]
  
instance Arbitrary IdpPolicyRuleActionProvider where
  arbitrary = sized genIdpPolicyRuleActionProvider

genIdpPolicyRuleActionProvider :: Int -> Gen IdpPolicyRuleActionProvider
genIdpPolicyRuleActionProvider n =
  IdpPolicyRuleActionProvider
    <$> arbitraryReducedMaybe n -- idpPolicyRuleActionProviderId :: Maybe Text
    <*> arbitraryReducedMaybe n -- idpPolicyRuleActionProviderType :: Maybe Text
  
instance Arbitrary ImageUploadResponse where
  arbitrary = sized genImageUploadResponse

genImageUploadResponse :: Int -> Gen ImageUploadResponse
genImageUploadResponse n =
  ImageUploadResponse
    <$> arbitraryReducedMaybe n -- imageUploadResponseUrl :: Maybe Text
  
instance Arbitrary InactivityPolicyRuleCondition where
  arbitrary = sized genInactivityPolicyRuleCondition

genInactivityPolicyRuleCondition :: Int -> Gen InactivityPolicyRuleCondition
genInactivityPolicyRuleCondition n =
  InactivityPolicyRuleCondition
    <$> arbitraryReducedMaybe n -- inactivityPolicyRuleConditionNumber :: Maybe Int
    <*> arbitraryReducedMaybe n -- inactivityPolicyRuleConditionUnit :: Maybe Text
  
instance Arbitrary InlineHook where
  arbitrary = sized genInlineHook

genInlineHook :: Int -> Gen InlineHook
genInlineHook n =
  InlineHook
    <$> arbitraryReducedMaybe n -- inlineHookLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- inlineHookChannel :: Maybe InlineHookChannel
    <*> arbitraryReducedMaybe n -- inlineHookCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- inlineHookId :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineHookLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- inlineHookName :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineHookStatus :: Maybe InlineHookStatus
    <*> arbitraryReducedMaybe n -- inlineHookType :: Maybe InlineHookType
    <*> arbitraryReducedMaybe n -- inlineHookVersion :: Maybe Text
  
instance Arbitrary InlineHookChannel where
  arbitrary = sized genInlineHookChannel

genInlineHookChannel :: Int -> Gen InlineHookChannel
genInlineHookChannel n =
  InlineHookChannel
    <$> arbitraryReducedMaybe n -- inlineHookChannelConfig :: Maybe InlineHookChannelConfig
    <*> arbitraryReducedMaybe n -- inlineHookChannelType :: Maybe E'Type2
    <*> arbitraryReducedMaybe n -- inlineHookChannelVersion :: Maybe Text
  
instance Arbitrary InlineHookChannelConfig where
  arbitrary = sized genInlineHookChannelConfig

genInlineHookChannelConfig :: Int -> Gen InlineHookChannelConfig
genInlineHookChannelConfig n =
  InlineHookChannelConfig
    <$> arbitraryReducedMaybe n -- inlineHookChannelConfigAuthScheme :: Maybe InlineHookChannelConfigAuthScheme
    <*> arbitraryReducedMaybe n -- inlineHookChannelConfigHeaders :: Maybe [InlineHookChannelConfigHeaders]
    <*> arbitraryReducedMaybe n -- inlineHookChannelConfigUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineHookChannelConfigMethod :: Maybe Text
  
instance Arbitrary InlineHookChannelConfigAuthScheme where
  arbitrary = sized genInlineHookChannelConfigAuthScheme

genInlineHookChannelConfigAuthScheme :: Int -> Gen InlineHookChannelConfigAuthScheme
genInlineHookChannelConfigAuthScheme n =
  InlineHookChannelConfigAuthScheme
    <$> arbitraryReducedMaybe n -- inlineHookChannelConfigAuthSchemeKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineHookChannelConfigAuthSchemeType :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineHookChannelConfigAuthSchemeValue :: Maybe Text
  
instance Arbitrary InlineHookChannelConfigHeaders where
  arbitrary = sized genInlineHookChannelConfigHeaders

genInlineHookChannelConfigHeaders :: Int -> Gen InlineHookChannelConfigHeaders
genInlineHookChannelConfigHeaders n =
  InlineHookChannelConfigHeaders
    <$> arbitraryReducedMaybe n -- inlineHookChannelConfigHeadersKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineHookChannelConfigHeadersValue :: Maybe Text
  
instance Arbitrary InlineHookResponse where
  arbitrary = sized genInlineHookResponse

genInlineHookResponse :: Int -> Gen InlineHookResponse
genInlineHookResponse n =
  InlineHookResponse
    <$> arbitraryReducedMaybe n -- inlineHookResponseCommands :: Maybe [InlineHookResponseCommands]
  
instance Arbitrary InlineHookResponseCommandValue where
  arbitrary = sized genInlineHookResponseCommandValue

genInlineHookResponseCommandValue :: Int -> Gen InlineHookResponseCommandValue
genInlineHookResponseCommandValue n =
  InlineHookResponseCommandValue
    <$> arbitraryReducedMaybe n -- inlineHookResponseCommandValueOp :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineHookResponseCommandValuePath :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineHookResponseCommandValueValue :: Maybe Text
  
instance Arbitrary InlineHookResponseCommands where
  arbitrary = sized genInlineHookResponseCommands

genInlineHookResponseCommands :: Int -> Gen InlineHookResponseCommands
genInlineHookResponseCommands n =
  InlineHookResponseCommands
    <$> arbitraryReducedMaybe n -- inlineHookResponseCommandsType :: Maybe Text
    <*> arbitraryReducedMaybe n -- inlineHookResponseCommandsValue :: Maybe [InlineHookResponseCommandValue]
  
instance Arbitrary IonField where
  arbitrary = sized genIonField

genIonField :: Int -> Gen IonField
genIonField n =
  IonField
    <$> arbitraryReducedMaybe n -- ionFieldForm :: Maybe IonForm
    <*> arbitraryReducedMaybe n -- ionFieldLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- ionFieldMutable :: Maybe Bool
    <*> arbitraryReducedMaybe n -- ionFieldName :: Maybe Text
    <*> arbitraryReducedMaybe n -- ionFieldRequired :: Maybe Bool
    <*> arbitraryReducedMaybe n -- ionFieldSecret :: Maybe Bool
    <*> arbitraryReducedMaybe n -- ionFieldType :: Maybe Text
    <*> arbitraryReducedMaybe n -- ionFieldValue :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- ionFieldVisible :: Maybe Bool
  
instance Arbitrary IonForm where
  arbitrary = sized genIonForm

genIonForm :: Int -> Gen IonForm
genIonForm n =
  IonForm
    <$> arbitraryReducedMaybe n -- ionFormAccepts :: Maybe Text
    <*> arbitraryReducedMaybe n -- ionFormHref :: Maybe Text
    <*> arbitraryReducedMaybe n -- ionFormMethod :: Maybe Text
    <*> arbitraryReducedMaybe n -- ionFormName :: Maybe Text
    <*> arbitraryReducedMaybe n -- ionFormProduces :: Maybe Text
    <*> arbitraryReducedMaybe n -- ionFormRefresh :: Maybe Int
    <*> arbitraryReducedMaybe n -- ionFormRel :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- ionFormRelatesTo :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- ionFormValue :: Maybe [IonField]
  
instance Arbitrary JsonWebKey where
  arbitrary = sized genJsonWebKey

genJsonWebKey :: Int -> Gen JsonWebKey
genJsonWebKey n =
  JsonWebKey
    <$> arbitraryReducedMaybe n -- jsonWebKeyLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- jsonWebKeyAlg :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- jsonWebKeyE :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyExpiresAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- jsonWebKeyKeyOps :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- jsonWebKeyKid :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyKty :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- jsonWebKeyN :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyUse :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyX5c :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- jsonWebKeyX5t :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyX5tS256 :: Maybe Text
    <*> arbitraryReducedMaybe n -- jsonWebKeyX5u :: Maybe Text
  
instance Arbitrary JwkUse where
  arbitrary = sized genJwkUse

genJwkUse :: Int -> Gen JwkUse
genJwkUse n =
  JwkUse
    <$> arbitraryReducedMaybe n -- jwkUseUse :: Maybe E'Use
  
instance Arbitrary LifecycleCreateSettingObject where
  arbitrary = sized genLifecycleCreateSettingObject

genLifecycleCreateSettingObject :: Int -> Gen LifecycleCreateSettingObject
genLifecycleCreateSettingObject n =
  LifecycleCreateSettingObject
    <$> arbitraryReducedMaybe n -- lifecycleCreateSettingObjectStatus :: Maybe EnabledStatus
  
instance Arbitrary LifecycleDeactivateSettingObject where
  arbitrary = sized genLifecycleDeactivateSettingObject

genLifecycleDeactivateSettingObject :: Int -> Gen LifecycleDeactivateSettingObject
genLifecycleDeactivateSettingObject n =
  LifecycleDeactivateSettingObject
    <$> arbitraryReducedMaybe n -- lifecycleDeactivateSettingObjectStatus :: Maybe EnabledStatus
  
instance Arbitrary LifecycleExpirationPolicyRuleCondition where
  arbitrary = sized genLifecycleExpirationPolicyRuleCondition

genLifecycleExpirationPolicyRuleCondition :: Int -> Gen LifecycleExpirationPolicyRuleCondition
genLifecycleExpirationPolicyRuleCondition n =
  LifecycleExpirationPolicyRuleCondition
    <$> arbitraryReducedMaybe n -- lifecycleExpirationPolicyRuleConditionLifecycleStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- lifecycleExpirationPolicyRuleConditionNumber :: Maybe Int
    <*> arbitraryReducedMaybe n -- lifecycleExpirationPolicyRuleConditionUnit :: Maybe Text
  
instance Arbitrary LinkedObject where
  arbitrary = sized genLinkedObject

genLinkedObject :: Int -> Gen LinkedObject
genLinkedObject n =
  LinkedObject
    <$> arbitraryReducedMaybe n -- linkedObjectLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- linkedObjectAssociated :: Maybe LinkedObjectDetails
    <*> arbitraryReducedMaybe n -- linkedObjectPrimary :: Maybe LinkedObjectDetails
  
instance Arbitrary LinkedObjectDetails where
  arbitrary = sized genLinkedObjectDetails

genLinkedObjectDetails :: Int -> Gen LinkedObjectDetails
genLinkedObjectDetails n =
  LinkedObjectDetails
    <$> arbitraryReducedMaybe n -- linkedObjectDetailsDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkedObjectDetailsName :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkedObjectDetailsTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- linkedObjectDetailsType :: Maybe LinkedObjectDetailsType
  
instance Arbitrary LogActor where
  arbitrary = sized genLogActor

genLogActor :: Int -> Gen LogActor
genLogActor n =
  LogActor
    <$> arbitraryReducedMaybe n -- logActorAlternateId :: Maybe Text
    <*> arbitraryReducedMaybe n -- logActorDetail :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- logActorDisplayName :: Maybe Text
    <*> arbitraryReducedMaybe n -- logActorId :: Maybe Text
    <*> arbitraryReducedMaybe n -- logActorType :: Maybe Text
  
instance Arbitrary LogAuthenticationContext where
  arbitrary = sized genLogAuthenticationContext

genLogAuthenticationContext :: Int -> Gen LogAuthenticationContext
genLogAuthenticationContext n =
  LogAuthenticationContext
    <$> arbitraryReducedMaybe n -- logAuthenticationContextAuthenticationProvider :: Maybe LogAuthenticationProvider
    <*> arbitraryReducedMaybe n -- logAuthenticationContextAuthenticationStep :: Maybe Int
    <*> arbitraryReducedMaybe n -- logAuthenticationContextCredentialProvider :: Maybe LogCredentialProvider
    <*> arbitraryReducedMaybe n -- logAuthenticationContextCredentialType :: Maybe LogCredentialType
    <*> arbitraryReducedMaybe n -- logAuthenticationContextExternalSessionId :: Maybe Text
    <*> arbitraryReducedMaybe n -- logAuthenticationContextInterface :: Maybe Text
    <*> arbitraryReducedMaybe n -- logAuthenticationContextIssuer :: Maybe LogIssuer
  
instance Arbitrary LogClient where
  arbitrary = sized genLogClient

genLogClient :: Int -> Gen LogClient
genLogClient n =
  LogClient
    <$> arbitraryReducedMaybe n -- logClientDevice :: Maybe Text
    <*> arbitraryReducedMaybe n -- logClientGeographicalContext :: Maybe LogGeographicalContext
    <*> arbitraryReducedMaybe n -- logClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- logClientIpAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- logClientUserAgent :: Maybe LogUserAgent
    <*> arbitraryReducedMaybe n -- logClientZone :: Maybe Text
  
instance Arbitrary LogDebugContext where
  arbitrary = sized genLogDebugContext

genLogDebugContext :: Int -> Gen LogDebugContext
genLogDebugContext n =
  LogDebugContext
    <$> arbitraryReducedMaybe n -- logDebugContextDebugData :: Maybe (Map.Map String A.Value)
  
instance Arbitrary LogEvent where
  arbitrary = sized genLogEvent

genLogEvent :: Int -> Gen LogEvent
genLogEvent n =
  LogEvent
    <$> arbitraryReducedMaybe n -- logEventActor :: Maybe LogActor
    <*> arbitraryReducedMaybe n -- logEventAuthenticationContext :: Maybe LogAuthenticationContext
    <*> arbitraryReducedMaybe n -- logEventClient :: Maybe LogClient
    <*> arbitraryReducedMaybe n -- logEventDebugContext :: Maybe LogDebugContext
    <*> arbitraryReducedMaybe n -- logEventDisplayMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- logEventEventType :: Maybe Text
    <*> arbitraryReducedMaybe n -- logEventLegacyEventType :: Maybe Text
    <*> arbitraryReducedMaybe n -- logEventOutcome :: Maybe LogOutcome
    <*> arbitraryReducedMaybe n -- logEventPublished :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- logEventRequest :: Maybe LogRequest
    <*> arbitraryReducedMaybe n -- logEventSecurityContext :: Maybe LogSecurityContext
    <*> arbitraryReducedMaybe n -- logEventSeverity :: Maybe LogSeverity
    <*> arbitraryReducedMaybe n -- logEventTarget :: Maybe [LogTarget]
    <*> arbitraryReducedMaybe n -- logEventTransaction :: Maybe LogTransaction
    <*> arbitraryReducedMaybe n -- logEventUuid :: Maybe Text
    <*> arbitraryReducedMaybe n -- logEventVersion :: Maybe Text
  
instance Arbitrary LogGeographicalContext where
  arbitrary = sized genLogGeographicalContext

genLogGeographicalContext :: Int -> Gen LogGeographicalContext
genLogGeographicalContext n =
  LogGeographicalContext
    <$> arbitraryReducedMaybe n -- logGeographicalContextCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- logGeographicalContextCountry :: Maybe Text
    <*> arbitraryReducedMaybe n -- logGeographicalContextGeolocation :: Maybe LogGeolocation
    <*> arbitraryReducedMaybe n -- logGeographicalContextPostalCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- logGeographicalContextState :: Maybe Text
  
instance Arbitrary LogGeolocation where
  arbitrary = sized genLogGeolocation

genLogGeolocation :: Int -> Gen LogGeolocation
genLogGeolocation n =
  LogGeolocation
    <$> arbitraryReducedMaybe n -- logGeolocationLat :: Maybe Double
    <*> arbitraryReducedMaybe n -- logGeolocationLon :: Maybe Double
  
instance Arbitrary LogIpAddress where
  arbitrary = sized genLogIpAddress

genLogIpAddress :: Int -> Gen LogIpAddress
genLogIpAddress n =
  LogIpAddress
    <$> arbitraryReducedMaybe n -- logIpAddressGeographicalContext :: Maybe LogGeographicalContext
    <*> arbitraryReducedMaybe n -- logIpAddressIp :: Maybe Text
    <*> arbitraryReducedMaybe n -- logIpAddressSource :: Maybe Text
    <*> arbitraryReducedMaybe n -- logIpAddressVersion :: Maybe Text
  
instance Arbitrary LogIssuer where
  arbitrary = sized genLogIssuer

genLogIssuer :: Int -> Gen LogIssuer
genLogIssuer n =
  LogIssuer
    <$> arbitraryReducedMaybe n -- logIssuerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- logIssuerType :: Maybe Text
  
instance Arbitrary LogOutcome where
  arbitrary = sized genLogOutcome

genLogOutcome :: Int -> Gen LogOutcome
genLogOutcome n =
  LogOutcome
    <$> arbitraryReducedMaybe n -- logOutcomeReason :: Maybe Text
    <*> arbitraryReducedMaybe n -- logOutcomeResult :: Maybe Text
  
instance Arbitrary LogRequest where
  arbitrary = sized genLogRequest

genLogRequest :: Int -> Gen LogRequest
genLogRequest n =
  LogRequest
    <$> arbitraryReducedMaybe n -- logRequestIpChain :: Maybe [LogIpAddress]
  
instance Arbitrary LogSecurityContext where
  arbitrary = sized genLogSecurityContext

genLogSecurityContext :: Int -> Gen LogSecurityContext
genLogSecurityContext n =
  LogSecurityContext
    <$> arbitraryReducedMaybe n -- logSecurityContextAsNumber :: Maybe Int
    <*> arbitraryReducedMaybe n -- logSecurityContextAsOrg :: Maybe Text
    <*> arbitraryReducedMaybe n -- logSecurityContextDomain :: Maybe Text
    <*> arbitraryReducedMaybe n -- logSecurityContextIsProxy :: Maybe Bool
    <*> arbitraryReducedMaybe n -- logSecurityContextIsp :: Maybe Text
  
instance Arbitrary LogTarget where
  arbitrary = sized genLogTarget

genLogTarget :: Int -> Gen LogTarget
genLogTarget n =
  LogTarget
    <$> arbitraryReducedMaybe n -- logTargetAlternateId :: Maybe Text
    <*> arbitraryReducedMaybe n -- logTargetDetailEntry :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- logTargetDisplayName :: Maybe Text
    <*> arbitraryReducedMaybe n -- logTargetId :: Maybe Text
    <*> arbitraryReducedMaybe n -- logTargetType :: Maybe Text
  
instance Arbitrary LogTransaction where
  arbitrary = sized genLogTransaction

genLogTransaction :: Int -> Gen LogTransaction
genLogTransaction n =
  LogTransaction
    <$> arbitraryReducedMaybe n -- logTransactionDetail :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- logTransactionId :: Maybe Text
    <*> arbitraryReducedMaybe n -- logTransactionType :: Maybe Text
  
instance Arbitrary LogUserAgent where
  arbitrary = sized genLogUserAgent

genLogUserAgent :: Int -> Gen LogUserAgent
genLogUserAgent n =
  LogUserAgent
    <$> arbitraryReducedMaybe n -- logUserAgentBrowser :: Maybe Text
    <*> arbitraryReducedMaybe n -- logUserAgentOs :: Maybe Text
    <*> arbitraryReducedMaybe n -- logUserAgentRawUserAgent :: Maybe Text
  
instance Arbitrary MDMEnrollmentPolicyRuleCondition where
  arbitrary = sized genMDMEnrollmentPolicyRuleCondition

genMDMEnrollmentPolicyRuleCondition :: Int -> Gen MDMEnrollmentPolicyRuleCondition
genMDMEnrollmentPolicyRuleCondition n =
  MDMEnrollmentPolicyRuleCondition
    <$> arbitraryReducedMaybe n -- mDMEnrollmentPolicyRuleConditionBlockNonSafeAndroid :: Maybe Bool
    <*> arbitraryReducedMaybe n -- mDMEnrollmentPolicyRuleConditionEnrollment :: Maybe E'Enrollment
  
instance Arbitrary MultifactorEnrollmentPolicy where
  arbitrary = sized genMultifactorEnrollmentPolicy

genMultifactorEnrollmentPolicy :: Int -> Gen MultifactorEnrollmentPolicy
genMultifactorEnrollmentPolicy n =
  MultifactorEnrollmentPolicy
    <$> arbitraryReducedMaybe n -- multifactorEnrollmentPolicyEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- multifactorEnrollmentPolicyLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- multifactorEnrollmentPolicyConditions :: Maybe PolicyRuleConditions
    <*> arbitraryReducedMaybe n -- multifactorEnrollmentPolicyCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- multifactorEnrollmentPolicyDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- multifactorEnrollmentPolicyId :: Maybe Text
    <*> arbitraryReducedMaybe n -- multifactorEnrollmentPolicyLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- multifactorEnrollmentPolicyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- multifactorEnrollmentPolicyPriority :: Maybe Int
    <*> arbitraryReducedMaybe n -- multifactorEnrollmentPolicyStatus :: Maybe E'Status2
    <*> arbitraryReducedMaybe n -- multifactorEnrollmentPolicySystem :: Maybe Bool
    <*> arbitraryReducedMaybe n -- multifactorEnrollmentPolicyType :: Maybe PolicyType
    <*> arbitraryReducedMaybe n -- multifactorEnrollmentPolicySettings :: Maybe MultifactorEnrollmentPolicySettings
  
instance Arbitrary MultifactorEnrollmentPolicyAuthenticatorSettings where
  arbitrary = sized genMultifactorEnrollmentPolicyAuthenticatorSettings

genMultifactorEnrollmentPolicyAuthenticatorSettings :: Int -> Gen MultifactorEnrollmentPolicyAuthenticatorSettings
genMultifactorEnrollmentPolicyAuthenticatorSettings n =
  MultifactorEnrollmentPolicyAuthenticatorSettings
    <$> arbitraryReducedMaybe n -- multifactorEnrollmentPolicyAuthenticatorSettingsConstraints :: Maybe MultifactorEnrollmentPolicyAuthenticatorSettingsConstraints
    <*> arbitraryReducedMaybe n -- multifactorEnrollmentPolicyAuthenticatorSettingsEnroll :: Maybe MultifactorEnrollmentPolicyAuthenticatorSettingsEnroll
    <*> arbitraryReducedMaybe n -- multifactorEnrollmentPolicyAuthenticatorSettingsKey :: Maybe MultifactorEnrollmentPolicyAuthenticatorType
  
instance Arbitrary MultifactorEnrollmentPolicyAuthenticatorSettingsConstraints where
  arbitrary = sized genMultifactorEnrollmentPolicyAuthenticatorSettingsConstraints

genMultifactorEnrollmentPolicyAuthenticatorSettingsConstraints :: Int -> Gen MultifactorEnrollmentPolicyAuthenticatorSettingsConstraints
genMultifactorEnrollmentPolicyAuthenticatorSettingsConstraints n =
  MultifactorEnrollmentPolicyAuthenticatorSettingsConstraints
    <$> arbitraryReducedMaybe n -- multifactorEnrollmentPolicyAuthenticatorSettingsConstraintsAaguidGroups :: Maybe [Text]
  
instance Arbitrary MultifactorEnrollmentPolicyAuthenticatorSettingsEnroll where
  arbitrary = sized genMultifactorEnrollmentPolicyAuthenticatorSettingsEnroll

genMultifactorEnrollmentPolicyAuthenticatorSettingsEnroll :: Int -> Gen MultifactorEnrollmentPolicyAuthenticatorSettingsEnroll
genMultifactorEnrollmentPolicyAuthenticatorSettingsEnroll n =
  MultifactorEnrollmentPolicyAuthenticatorSettingsEnroll
    <$> arbitraryReducedMaybe n -- multifactorEnrollmentPolicyAuthenticatorSettingsEnrollSelf :: Maybe MultifactorEnrollmentPolicyAuthenticatorStatus
  
instance Arbitrary MultifactorEnrollmentPolicySettings where
  arbitrary = sized genMultifactorEnrollmentPolicySettings

genMultifactorEnrollmentPolicySettings :: Int -> Gen MultifactorEnrollmentPolicySettings
genMultifactorEnrollmentPolicySettings n =
  MultifactorEnrollmentPolicySettings
    <$> arbitraryReducedMaybe n -- multifactorEnrollmentPolicySettingsAuthenticators :: Maybe [MultifactorEnrollmentPolicyAuthenticatorSettings]
    <*> arbitraryReducedMaybe n -- multifactorEnrollmentPolicySettingsType :: Maybe MultifactorEnrollmentPolicySettingsType
  
instance Arbitrary NetworkZone where
  arbitrary = sized genNetworkZone

genNetworkZone :: Int -> Gen NetworkZone
genNetworkZone n =
  NetworkZone
    <$> arbitraryReducedMaybe n -- networkZoneType :: Maybe NetworkZoneType
    <*> arbitraryReducedMaybe n -- networkZoneId :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkZoneName :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkZoneSystem :: Maybe Bool
    <*> arbitraryReducedMaybe n -- networkZoneUsage :: Maybe NetworkZoneUsage
    <*> arbitraryReducedMaybe n -- networkZoneStatus :: Maybe NetworkZoneStatus
    <*> arbitraryReducedMaybe n -- networkZoneProxyType :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkZoneLocations :: Maybe [NetworkZoneLocation]
    <*> arbitraryReducedMaybe n -- networkZoneGateways :: Maybe [NetworkZoneAddress]
    <*> arbitraryReducedMaybe n -- networkZoneProxies :: Maybe [NetworkZoneAddress]
    <*> arbitraryReducedMaybe n -- networkZoneAsns :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- networkZoneCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- networkZoneLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- networkZoneLinks :: Maybe (Map.Map String A.Value)
  
instance Arbitrary NetworkZoneAddress where
  arbitrary = sized genNetworkZoneAddress

genNetworkZoneAddress :: Int -> Gen NetworkZoneAddress
genNetworkZoneAddress n =
  NetworkZoneAddress
    <$> arbitraryReducedMaybe n -- networkZoneAddressType :: Maybe NetworkZoneAddressType
    <*> arbitraryReducedMaybe n -- networkZoneAddressValue :: Maybe Text
  
instance Arbitrary NetworkZoneLocation where
  arbitrary = sized genNetworkZoneLocation

genNetworkZoneLocation :: Int -> Gen NetworkZoneLocation
genNetworkZoneLocation n =
  NetworkZoneLocation
    <$> arbitraryReducedMaybe n -- networkZoneLocationCountry :: Maybe Text
    <*> arbitraryReducedMaybe n -- networkZoneLocationRegion :: Maybe Text
  
instance Arbitrary OAuth2Actor where
  arbitrary = sized genOAuth2Actor

genOAuth2Actor :: Int -> Gen OAuth2Actor
genOAuth2Actor n =
  OAuth2Actor
    <$> arbitraryReducedMaybe n -- oAuth2ActorId :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ActorType :: Maybe Text
  
instance Arbitrary OAuth2Claim where
  arbitrary = sized genOAuth2Claim

genOAuth2Claim :: Int -> Gen OAuth2Claim
genOAuth2Claim n =
  OAuth2Claim
    <$> arbitraryReducedMaybe n -- oAuth2ClaimLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- oAuth2ClaimAlwaysIncludeInToken :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oAuth2ClaimClaimType :: Maybe E'ClaimType
    <*> arbitraryReducedMaybe n -- oAuth2ClaimConditions :: Maybe OAuth2ClaimConditions
    <*> arbitraryReducedMaybe n -- oAuth2ClaimGroupFilterType :: Maybe E'GroupFilterType
    <*> arbitraryReducedMaybe n -- oAuth2ClaimId :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClaimName :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClaimStatus :: Maybe E'Status2
    <*> arbitraryReducedMaybe n -- oAuth2ClaimSystem :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oAuth2ClaimValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClaimValueType :: Maybe E'ValueType
  
instance Arbitrary OAuth2ClaimConditions where
  arbitrary = sized genOAuth2ClaimConditions

genOAuth2ClaimConditions :: Int -> Gen OAuth2ClaimConditions
genOAuth2ClaimConditions n =
  OAuth2ClaimConditions
    <$> arbitraryReducedMaybe n -- oAuth2ClaimConditionsScopes :: Maybe [Text]
  
instance Arbitrary OAuth2Client where
  arbitrary = sized genOAuth2Client

genOAuth2Client :: Int -> Gen OAuth2Client
genOAuth2Client n =
  OAuth2Client
    <$> arbitraryReducedMaybe n -- oAuth2ClientLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- oAuth2ClientClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientClientName :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientClientUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ClientLogoUri :: Maybe Text
  
instance Arbitrary OAuth2RefreshToken where
  arbitrary = sized genOAuth2RefreshToken

genOAuth2RefreshToken :: Int -> Gen OAuth2RefreshToken
genOAuth2RefreshToken n =
  OAuth2RefreshToken
    <$> arbitraryReducedMaybe n -- oAuth2RefreshTokenEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- oAuth2RefreshTokenLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- oAuth2RefreshTokenClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2RefreshTokenCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- oAuth2RefreshTokenCreatedBy :: Maybe OAuth2Actor
    <*> arbitraryReducedMaybe n -- oAuth2RefreshTokenExpiresAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- oAuth2RefreshTokenId :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2RefreshTokenIssuer :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2RefreshTokenLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- oAuth2RefreshTokenScopes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oAuth2RefreshTokenStatus :: Maybe E'Status3
    <*> arbitraryReducedMaybe n -- oAuth2RefreshTokenUserId :: Maybe Text
  
instance Arbitrary OAuth2Scope where
  arbitrary = sized genOAuth2Scope

genOAuth2Scope :: Int -> Gen OAuth2Scope
genOAuth2Scope n =
  OAuth2Scope
    <$> arbitraryReducedMaybe n -- oAuth2ScopeConsent :: Maybe E'Consent
    <*> arbitraryReducedMaybe n -- oAuth2ScopeDefault :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oAuth2ScopeDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ScopeDisplayName :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ScopeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ScopeMetadataPublish :: Maybe E'MetadataPublish
    <*> arbitraryReducedMaybe n -- oAuth2ScopeName :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ScopeSystem :: Maybe Bool
  
instance Arbitrary OAuth2ScopeConsentGrant where
  arbitrary = sized genOAuth2ScopeConsentGrant

genOAuth2ScopeConsentGrant :: Int -> Gen OAuth2ScopeConsentGrant
genOAuth2ScopeConsentGrant n =
  OAuth2ScopeConsentGrant
    <$> arbitraryReducedMaybe n -- oAuth2ScopeConsentGrantEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- oAuth2ScopeConsentGrantLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- oAuth2ScopeConsentGrantClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ScopeConsentGrantUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ScopeConsentGrantCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- oAuth2ScopeConsentGrantCreatedBy :: Maybe OAuth2Actor
    <*> arbitraryReducedMaybe n -- oAuth2ScopeConsentGrantId :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ScopeConsentGrantIssuer :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ScopeConsentGrantLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- oAuth2ScopeConsentGrantScopeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2ScopeConsentGrantSource :: Maybe OAuth2ScopeConsentGrantSource
    <*> arbitraryReducedMaybe n -- oAuth2ScopeConsentGrantStatus :: Maybe OAuth2ScopeConsentGrantStatus
  
instance Arbitrary OAuth2ScopesMediationPolicyRuleCondition where
  arbitrary = sized genOAuth2ScopesMediationPolicyRuleCondition

genOAuth2ScopesMediationPolicyRuleCondition :: Int -> Gen OAuth2ScopesMediationPolicyRuleCondition
genOAuth2ScopesMediationPolicyRuleCondition n =
  OAuth2ScopesMediationPolicyRuleCondition
    <$> arbitraryReducedMaybe n -- oAuth2ScopesMediationPolicyRuleConditionInclude :: Maybe [Text]
  
instance Arbitrary OAuth2Token where
  arbitrary = sized genOAuth2Token

genOAuth2Token :: Int -> Gen OAuth2Token
genOAuth2Token n =
  OAuth2Token
    <$> arbitraryReducedMaybe n -- oAuth2TokenEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- oAuth2TokenLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- oAuth2TokenClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2TokenCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- oAuth2TokenExpiresAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- oAuth2TokenId :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2TokenIssuer :: Maybe Text
    <*> arbitraryReducedMaybe n -- oAuth2TokenLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- oAuth2TokenScopes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- oAuth2TokenStatus :: Maybe E'Status3
    <*> arbitraryReducedMaybe n -- oAuth2TokenUserId :: Maybe Text
  
instance Arbitrary OAuthApplicationCredentials where
  arbitrary = sized genOAuthApplicationCredentials

genOAuthApplicationCredentials :: Int -> Gen OAuthApplicationCredentials
genOAuthApplicationCredentials n =
  OAuthApplicationCredentials
    <$> arbitraryReducedMaybe n -- oAuthApplicationCredentialsOauthClient :: Maybe ApplicationCredentialsOAuthClient
  
instance Arbitrary OktaSignOnPolicy where
  arbitrary = sized genOktaSignOnPolicy

genOktaSignOnPolicy :: Int -> Gen OktaSignOnPolicy
genOktaSignOnPolicy n =
  OktaSignOnPolicy
    <$> arbitraryReducedMaybe n -- oktaSignOnPolicyConditions :: Maybe OktaSignOnPolicyConditions
  
instance Arbitrary OktaSignOnPolicyConditions where
  arbitrary = sized genOktaSignOnPolicyConditions

genOktaSignOnPolicyConditions :: Int -> Gen OktaSignOnPolicyConditions
genOktaSignOnPolicyConditions n =
  OktaSignOnPolicyConditions
    <$> arbitraryReducedMaybe n -- oktaSignOnPolicyConditionsPeople :: Maybe PolicyPeopleCondition
  
instance Arbitrary OktaSignOnPolicyRule where
  arbitrary = sized genOktaSignOnPolicyRule

genOktaSignOnPolicyRule :: Int -> Gen OktaSignOnPolicyRule
genOktaSignOnPolicyRule n =
  OktaSignOnPolicyRule
    <$> arbitraryReducedMaybe n -- oktaSignOnPolicyRuleActions :: Maybe OktaSignOnPolicyRuleActions
    <*> arbitraryReducedMaybe n -- oktaSignOnPolicyRuleConditions :: Maybe OktaSignOnPolicyRuleConditions
    <*> arbitraryReducedMaybe n -- oktaSignOnPolicyRuleName :: Maybe Text
  
instance Arbitrary OktaSignOnPolicyRuleActions where
  arbitrary = sized genOktaSignOnPolicyRuleActions

genOktaSignOnPolicyRuleActions :: Int -> Gen OktaSignOnPolicyRuleActions
genOktaSignOnPolicyRuleActions n =
  OktaSignOnPolicyRuleActions
    <$> arbitraryReducedMaybe n -- oktaSignOnPolicyRuleActionsSignon :: Maybe OktaSignOnPolicyRuleSignonActions
  
instance Arbitrary OktaSignOnPolicyRuleConditions where
  arbitrary = sized genOktaSignOnPolicyRuleConditions

genOktaSignOnPolicyRuleConditions :: Int -> Gen OktaSignOnPolicyRuleConditions
genOktaSignOnPolicyRuleConditions n =
  OktaSignOnPolicyRuleConditions
    <$> arbitraryReducedMaybe n -- oktaSignOnPolicyRuleConditionsAuthContext :: Maybe PolicyRuleAuthContextCondition
    <*> arbitraryReducedMaybe n -- oktaSignOnPolicyRuleConditionsNetwork :: Maybe PolicyNetworkCondition
    <*> arbitraryReducedMaybe n -- oktaSignOnPolicyRuleConditionsPeople :: Maybe PolicyPeopleCondition
  
instance Arbitrary OktaSignOnPolicyRuleSignonActions where
  arbitrary = sized genOktaSignOnPolicyRuleSignonActions

genOktaSignOnPolicyRuleSignonActions :: Int -> Gen OktaSignOnPolicyRuleSignonActions
genOktaSignOnPolicyRuleSignonActions n =
  OktaSignOnPolicyRuleSignonActions
    <$> arbitraryReducedMaybe n -- oktaSignOnPolicyRuleSignonActionsAccess :: Maybe E'Access
    <*> arbitraryReducedMaybe n -- oktaSignOnPolicyRuleSignonActionsFactorLifetime :: Maybe Int
    <*> arbitraryReducedMaybe n -- oktaSignOnPolicyRuleSignonActionsFactorPromptMode :: Maybe E'FactorPromptMode
    <*> arbitraryReducedMaybe n -- oktaSignOnPolicyRuleSignonActionsRememberDeviceByDefault :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oktaSignOnPolicyRuleSignonActionsRequireFactor :: Maybe Bool
    <*> arbitraryReducedMaybe n -- oktaSignOnPolicyRuleSignonActionsSession :: Maybe OktaSignOnPolicyRuleSignonSessionActions
  
instance Arbitrary OktaSignOnPolicyRuleSignonSessionActions where
  arbitrary = sized genOktaSignOnPolicyRuleSignonSessionActions

genOktaSignOnPolicyRuleSignonSessionActions :: Int -> Gen OktaSignOnPolicyRuleSignonSessionActions
genOktaSignOnPolicyRuleSignonSessionActions n =
  OktaSignOnPolicyRuleSignonSessionActions
    <$> arbitraryReducedMaybe n -- oktaSignOnPolicyRuleSignonSessionActionsMaxSessionIdleMinutes :: Maybe Int
    <*> arbitraryReducedMaybe n -- oktaSignOnPolicyRuleSignonSessionActionsMaxSessionLifetimeMinutes :: Maybe Int
    <*> arbitraryReducedMaybe n -- oktaSignOnPolicyRuleSignonSessionActionsUsePersistentCookie :: Maybe Bool
  
instance Arbitrary OpenIdConnectApplication where
  arbitrary = sized genOpenIdConnectApplication

genOpenIdConnectApplication :: Int -> Gen OpenIdConnectApplication
genOpenIdConnectApplication n =
  OpenIdConnectApplication
    <$> arbitraryReducedMaybe n -- openIdConnectApplicationCredentials :: Maybe OAuthApplicationCredentials
    <*> arbitraryReducedMaybeValue n -- openIdConnectApplicationName :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettings :: Maybe OpenIdConnectApplicationSettings
  
instance Arbitrary OpenIdConnectApplicationIdpInitiatedLogin where
  arbitrary = sized genOpenIdConnectApplicationIdpInitiatedLogin

genOpenIdConnectApplicationIdpInitiatedLogin :: Int -> Gen OpenIdConnectApplicationIdpInitiatedLogin
genOpenIdConnectApplicationIdpInitiatedLogin n =
  OpenIdConnectApplicationIdpInitiatedLogin
    <$> arbitraryReducedMaybe n -- openIdConnectApplicationIdpInitiatedLoginMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationIdpInitiatedLoginDefaultScope :: Maybe [Text]
  
instance Arbitrary OpenIdConnectApplicationSettings where
  arbitrary = sized genOpenIdConnectApplicationSettings

genOpenIdConnectApplicationSettings :: Int -> Gen OpenIdConnectApplicationSettings
genOpenIdConnectApplicationSettings n =
  OpenIdConnectApplicationSettings
    <$> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsOauthClient :: Maybe OpenIdConnectApplicationSettingsClient
  
instance Arbitrary OpenIdConnectApplicationSettingsClient where
  arbitrary = sized genOpenIdConnectApplicationSettingsClient

genOpenIdConnectApplicationSettingsClient :: Int -> Gen OpenIdConnectApplicationSettingsClient
genOpenIdConnectApplicationSettingsClient n =
  OpenIdConnectApplicationSettingsClient
    <$> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientApplicationType :: Maybe OpenIdConnectApplicationType
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientClientUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientConsentMethod :: Maybe OpenIdConnectApplicationConsentMethod
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientGrantTypes :: Maybe [OAuthGrantType]
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientInitiateLoginUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientIssuerMode :: Maybe OpenIdConnectApplicationIssuerMode
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientIdpInitiatedLogin :: Maybe OpenIdConnectApplicationIdpInitiatedLogin
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientLogoUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientPolicyUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientPostLogoutRedirectUris :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientRedirectUris :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientWildcardRedirect :: Maybe Text
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientResponseTypes :: Maybe [OAuthResponseType]
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientRefreshToken :: Maybe OpenIdConnectApplicationSettingsRefreshToken
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientTosUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientJwks :: Maybe OpenIdConnectApplicationSettingsClientKeys
  
instance Arbitrary OpenIdConnectApplicationSettingsClientKeys where
  arbitrary = sized genOpenIdConnectApplicationSettingsClientKeys

genOpenIdConnectApplicationSettingsClientKeys :: Int -> Gen OpenIdConnectApplicationSettingsClientKeys
genOpenIdConnectApplicationSettingsClientKeys n =
  OpenIdConnectApplicationSettingsClientKeys
    <$> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientKeysKeys :: Maybe [JsonWebKey]
  
instance Arbitrary OpenIdConnectApplicationSettingsRefreshToken where
  arbitrary = sized genOpenIdConnectApplicationSettingsRefreshToken

genOpenIdConnectApplicationSettingsRefreshToken :: Int -> Gen OpenIdConnectApplicationSettingsRefreshToken
genOpenIdConnectApplicationSettingsRefreshToken n =
  OpenIdConnectApplicationSettingsRefreshToken
    <$> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsRefreshTokenLeeway :: Maybe Int
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsRefreshTokenRotationType :: Maybe OpenIdConnectRefreshTokenRotationType
  
instance Arbitrary Org2OrgApplication where
  arbitrary = sized genOrg2OrgApplication

genOrg2OrgApplication :: Int -> Gen Org2OrgApplication
genOrg2OrgApplication n =
  Org2OrgApplication
    <$> arbitraryReducedMaybeValue n -- org2OrgApplicationName :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- org2OrgApplicationSettings :: Maybe Org2OrgApplicationSettings
  
instance Arbitrary Org2OrgApplicationSettings where
  arbitrary = sized genOrg2OrgApplicationSettings

genOrg2OrgApplicationSettings :: Int -> Gen Org2OrgApplicationSettings
genOrg2OrgApplicationSettings n =
  Org2OrgApplicationSettings
    <$> arbitraryReducedMaybe n -- org2OrgApplicationSettingsApp :: Maybe Org2OrgApplicationSettingsApp
  
instance Arbitrary Org2OrgApplicationSettingsApp where
  arbitrary = sized genOrg2OrgApplicationSettingsApp

genOrg2OrgApplicationSettingsApp :: Int -> Gen Org2OrgApplicationSettingsApp
genOrg2OrgApplicationSettingsApp n =
  Org2OrgApplicationSettingsApp
    <$> arbitraryReducedMaybe n -- org2OrgApplicationSettingsAppAcsUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- org2OrgApplicationSettingsAppAudRestriction :: Maybe Text
    <*> arbitraryReducedMaybe n -- org2OrgApplicationSettingsAppBaseUrl :: Maybe Text
  
instance Arbitrary OrgContactTypeObj where
  arbitrary = sized genOrgContactTypeObj

genOrgContactTypeObj :: Int -> Gen OrgContactTypeObj
genOrgContactTypeObj n =
  OrgContactTypeObj
    <$> arbitraryReducedMaybeValue n -- orgContactTypeObjLinks :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- orgContactTypeObjContactType :: Maybe OrgContactType
  
instance Arbitrary OrgContactUser where
  arbitrary = sized genOrgContactUser

genOrgContactUser :: Int -> Gen OrgContactUser
genOrgContactUser n =
  OrgContactUser
    <$> arbitraryReducedMaybe n -- orgContactUserLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- orgContactUserUserId :: Maybe Text
  
instance Arbitrary OrgOktaCommunicationSetting where
  arbitrary = sized genOrgOktaCommunicationSetting

genOrgOktaCommunicationSetting :: Int -> Gen OrgOktaCommunicationSetting
genOrgOktaCommunicationSetting n =
  OrgOktaCommunicationSetting
    <$> arbitraryReducedMaybeValue n -- orgOktaCommunicationSettingLinks :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- orgOktaCommunicationSettingOptOutEmailUsers :: Maybe Bool
  
instance Arbitrary OrgOktaSupportSettingsObj where
  arbitrary = sized genOrgOktaSupportSettingsObj

genOrgOktaSupportSettingsObj :: Int -> Gen OrgOktaSupportSettingsObj
genOrgOktaSupportSettingsObj n =
  OrgOktaSupportSettingsObj
    <$> arbitraryReducedMaybeValue n -- orgOktaSupportSettingsObjLinks :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- orgOktaSupportSettingsObjExpiration :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- orgOktaSupportSettingsObjSupport :: Maybe OrgOktaSupportSetting
  
instance Arbitrary OrgPreferences where
  arbitrary = sized genOrgPreferences

genOrgPreferences :: Int -> Gen OrgPreferences
genOrgPreferences n =
  OrgPreferences
    <$> arbitraryReducedMaybeValue n -- orgPreferencesLinks :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- orgPreferencesShowEndUserFooter :: Maybe Bool
  
instance Arbitrary OrgSetting where
  arbitrary = sized genOrgSetting

genOrgSetting :: Int -> Gen OrgSetting
genOrgSetting n =
  OrgSetting
    <$> arbitraryReducedMaybeValue n -- orgSettingLinks :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- orgSettingAddress1 :: Maybe Text
    <*> arbitraryReducedMaybe n -- orgSettingAddress2 :: Maybe Text
    <*> arbitraryReducedMaybe n -- orgSettingCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- orgSettingCompanyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- orgSettingCountry :: Maybe Text
    <*> arbitraryReducedMaybe n -- orgSettingCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- orgSettingEndUserSupportHelpUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- orgSettingExpiresAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- orgSettingId :: Maybe Text
    <*> arbitraryReducedMaybe n -- orgSettingLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- orgSettingPhoneNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- orgSettingPostalCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- orgSettingState :: Maybe Text
    <*> arbitraryReducedMaybe n -- orgSettingStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- orgSettingSubdomain :: Maybe Text
    <*> arbitraryReducedMaybe n -- orgSettingSupportPhoneNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- orgSettingWebsite :: Maybe Text
  
instance Arbitrary PasswordCredential where
  arbitrary = sized genPasswordCredential

genPasswordCredential :: Int -> Gen PasswordCredential
genPasswordCredential n =
  PasswordCredential
    <$> arbitraryReducedMaybe n -- passwordCredentialHash :: Maybe PasswordCredentialHash
    <*> arbitraryReducedMaybe n -- passwordCredentialHook :: Maybe PasswordCredentialHook
    <*> arbitraryReducedMaybe n -- passwordCredentialValue :: Maybe Text
  
instance Arbitrary PasswordCredentialHash where
  arbitrary = sized genPasswordCredentialHash

genPasswordCredentialHash :: Int -> Gen PasswordCredentialHash
genPasswordCredentialHash n =
  PasswordCredentialHash
    <$> arbitraryReducedMaybe n -- passwordCredentialHashAlgorithm :: Maybe PasswordCredentialHashAlgorithm
    <*> arbitraryReducedMaybe n -- passwordCredentialHashSalt :: Maybe Text
    <*> arbitraryReducedMaybe n -- passwordCredentialHashSaltOrder :: Maybe Text
    <*> arbitraryReducedMaybe n -- passwordCredentialHashValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- passwordCredentialHashWorkFactor :: Maybe Int
  
instance Arbitrary PasswordCredentialHook where
  arbitrary = sized genPasswordCredentialHook

genPasswordCredentialHook :: Int -> Gen PasswordCredentialHook
genPasswordCredentialHook n =
  PasswordCredentialHook
    <$> arbitraryReducedMaybe n -- passwordCredentialHookType :: Maybe Text
  
instance Arbitrary PasswordDictionary where
  arbitrary = sized genPasswordDictionary

genPasswordDictionary :: Int -> Gen PasswordDictionary
genPasswordDictionary n =
  PasswordDictionary
    <$> arbitraryReducedMaybe n -- passwordDictionaryCommon :: Maybe PasswordDictionaryCommon
  
instance Arbitrary PasswordDictionaryCommon where
  arbitrary = sized genPasswordDictionaryCommon

genPasswordDictionaryCommon :: Int -> Gen PasswordDictionaryCommon
genPasswordDictionaryCommon n =
  PasswordDictionaryCommon
    <$> arbitraryReducedMaybe n -- passwordDictionaryCommonExclude :: Maybe Bool
  
instance Arbitrary PasswordExpirationPolicyRuleCondition where
  arbitrary = sized genPasswordExpirationPolicyRuleCondition

genPasswordExpirationPolicyRuleCondition :: Int -> Gen PasswordExpirationPolicyRuleCondition
genPasswordExpirationPolicyRuleCondition n =
  PasswordExpirationPolicyRuleCondition
    <$> arbitraryReducedMaybe n -- passwordExpirationPolicyRuleConditionNumber :: Maybe Int
    <*> arbitraryReducedMaybe n -- passwordExpirationPolicyRuleConditionUnit :: Maybe Text
  
instance Arbitrary PasswordPolicy where
  arbitrary = sized genPasswordPolicy

genPasswordPolicy :: Int -> Gen PasswordPolicy
genPasswordPolicy n =
  PasswordPolicy
    <$> arbitraryReducedMaybe n -- passwordPolicyConditions :: Maybe PasswordPolicyConditions
    <*> arbitraryReducedMaybe n -- passwordPolicySettings :: Maybe PasswordPolicySettings
  
instance Arbitrary PasswordPolicyAuthenticationProviderCondition where
  arbitrary = sized genPasswordPolicyAuthenticationProviderCondition

genPasswordPolicyAuthenticationProviderCondition :: Int -> Gen PasswordPolicyAuthenticationProviderCondition
genPasswordPolicyAuthenticationProviderCondition n =
  PasswordPolicyAuthenticationProviderCondition
    <$> arbitraryReducedMaybe n -- passwordPolicyAuthenticationProviderConditionInclude :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- passwordPolicyAuthenticationProviderConditionProvider :: Maybe E'Provider2
  
instance Arbitrary PasswordPolicyConditions where
  arbitrary = sized genPasswordPolicyConditions

genPasswordPolicyConditions :: Int -> Gen PasswordPolicyConditions
genPasswordPolicyConditions n =
  PasswordPolicyConditions
    <$> arbitraryReducedMaybe n -- passwordPolicyConditionsAuthProvider :: Maybe PasswordPolicyAuthenticationProviderCondition
    <*> arbitraryReducedMaybe n -- passwordPolicyConditionsPeople :: Maybe PolicyPeopleCondition
  
instance Arbitrary PasswordPolicyDelegationSettings where
  arbitrary = sized genPasswordPolicyDelegationSettings

genPasswordPolicyDelegationSettings :: Int -> Gen PasswordPolicyDelegationSettings
genPasswordPolicyDelegationSettings n =
  PasswordPolicyDelegationSettings
    <$> arbitraryReducedMaybe n -- passwordPolicyDelegationSettingsOptions :: Maybe PasswordPolicyDelegationSettingsOptions
  
instance Arbitrary PasswordPolicyDelegationSettingsOptions where
  arbitrary = sized genPasswordPolicyDelegationSettingsOptions

genPasswordPolicyDelegationSettingsOptions :: Int -> Gen PasswordPolicyDelegationSettingsOptions
genPasswordPolicyDelegationSettingsOptions n =
  PasswordPolicyDelegationSettingsOptions
    <$> arbitraryReducedMaybe n -- passwordPolicyDelegationSettingsOptionsSkipUnlock :: Maybe Bool
  
instance Arbitrary PasswordPolicyPasswordSettings where
  arbitrary = sized genPasswordPolicyPasswordSettings

genPasswordPolicyPasswordSettings :: Int -> Gen PasswordPolicyPasswordSettings
genPasswordPolicyPasswordSettings n =
  PasswordPolicyPasswordSettings
    <$> arbitraryReducedMaybe n -- passwordPolicyPasswordSettingsAge :: Maybe PasswordPolicyPasswordSettingsAge
    <*> arbitraryReducedMaybe n -- passwordPolicyPasswordSettingsComplexity :: Maybe PasswordPolicyPasswordSettingsComplexity
    <*> arbitraryReducedMaybe n -- passwordPolicyPasswordSettingsLockout :: Maybe PasswordPolicyPasswordSettingsLockout
  
instance Arbitrary PasswordPolicyPasswordSettingsAge where
  arbitrary = sized genPasswordPolicyPasswordSettingsAge

genPasswordPolicyPasswordSettingsAge :: Int -> Gen PasswordPolicyPasswordSettingsAge
genPasswordPolicyPasswordSettingsAge n =
  PasswordPolicyPasswordSettingsAge
    <$> arbitraryReducedMaybe n -- passwordPolicyPasswordSettingsAgeExpireWarnDays :: Maybe Int
    <*> arbitraryReducedMaybe n -- passwordPolicyPasswordSettingsAgeHistoryCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- passwordPolicyPasswordSettingsAgeMaxAgeDays :: Maybe Int
    <*> arbitraryReducedMaybe n -- passwordPolicyPasswordSettingsAgeMinAgeMinutes :: Maybe Int
  
instance Arbitrary PasswordPolicyPasswordSettingsComplexity where
  arbitrary = sized genPasswordPolicyPasswordSettingsComplexity

genPasswordPolicyPasswordSettingsComplexity :: Int -> Gen PasswordPolicyPasswordSettingsComplexity
genPasswordPolicyPasswordSettingsComplexity n =
  PasswordPolicyPasswordSettingsComplexity
    <$> arbitraryReducedMaybe n -- passwordPolicyPasswordSettingsComplexityDictionary :: Maybe PasswordDictionary
    <*> arbitraryReducedMaybe n -- passwordPolicyPasswordSettingsComplexityExcludeAttributes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- passwordPolicyPasswordSettingsComplexityExcludeUsername :: Maybe Bool
    <*> arbitraryReducedMaybe n -- passwordPolicyPasswordSettingsComplexityMinLength :: Maybe Int
    <*> arbitraryReducedMaybe n -- passwordPolicyPasswordSettingsComplexityMinLowerCase :: Maybe Int
    <*> arbitraryReducedMaybe n -- passwordPolicyPasswordSettingsComplexityMinNumber :: Maybe Int
    <*> arbitraryReducedMaybe n -- passwordPolicyPasswordSettingsComplexityMinSymbol :: Maybe Int
    <*> arbitraryReducedMaybe n -- passwordPolicyPasswordSettingsComplexityMinUpperCase :: Maybe Int
  
instance Arbitrary PasswordPolicyPasswordSettingsLockout where
  arbitrary = sized genPasswordPolicyPasswordSettingsLockout

genPasswordPolicyPasswordSettingsLockout :: Int -> Gen PasswordPolicyPasswordSettingsLockout
genPasswordPolicyPasswordSettingsLockout n =
  PasswordPolicyPasswordSettingsLockout
    <$> arbitraryReducedMaybe n -- passwordPolicyPasswordSettingsLockoutAutoUnlockMinutes :: Maybe Int
    <*> arbitraryReducedMaybe n -- passwordPolicyPasswordSettingsLockoutMaxAttempts :: Maybe Int
    <*> arbitraryReducedMaybe n -- passwordPolicyPasswordSettingsLockoutShowLockoutFailures :: Maybe Bool
    <*> arbitraryReducedMaybe n -- passwordPolicyPasswordSettingsLockoutUserLockoutNotificationChannels :: Maybe [Text]
  
instance Arbitrary PasswordPolicyRecoveryEmail where
  arbitrary = sized genPasswordPolicyRecoveryEmail

genPasswordPolicyRecoveryEmail :: Int -> Gen PasswordPolicyRecoveryEmail
genPasswordPolicyRecoveryEmail n =
  PasswordPolicyRecoveryEmail
    <$> arbitraryReducedMaybe n -- passwordPolicyRecoveryEmailProperties :: Maybe PasswordPolicyRecoveryEmailProperties
    <*> arbitraryReducedMaybe n -- passwordPolicyRecoveryEmailStatus :: Maybe E'Status2
  
instance Arbitrary PasswordPolicyRecoveryEmailProperties where
  arbitrary = sized genPasswordPolicyRecoveryEmailProperties

genPasswordPolicyRecoveryEmailProperties :: Int -> Gen PasswordPolicyRecoveryEmailProperties
genPasswordPolicyRecoveryEmailProperties n =
  PasswordPolicyRecoveryEmailProperties
    <$> arbitraryReducedMaybe n -- passwordPolicyRecoveryEmailPropertiesRecoveryToken :: Maybe PasswordPolicyRecoveryEmailRecoveryToken
  
instance Arbitrary PasswordPolicyRecoveryEmailRecoveryToken where
  arbitrary = sized genPasswordPolicyRecoveryEmailRecoveryToken

genPasswordPolicyRecoveryEmailRecoveryToken :: Int -> Gen PasswordPolicyRecoveryEmailRecoveryToken
genPasswordPolicyRecoveryEmailRecoveryToken n =
  PasswordPolicyRecoveryEmailRecoveryToken
    <$> arbitraryReducedMaybe n -- passwordPolicyRecoveryEmailRecoveryTokenTokenLifetimeMinutes :: Maybe Int
  
instance Arbitrary PasswordPolicyRecoveryFactorSettings where
  arbitrary = sized genPasswordPolicyRecoveryFactorSettings

genPasswordPolicyRecoveryFactorSettings :: Int -> Gen PasswordPolicyRecoveryFactorSettings
genPasswordPolicyRecoveryFactorSettings n =
  PasswordPolicyRecoveryFactorSettings
    <$> arbitraryReducedMaybe n -- passwordPolicyRecoveryFactorSettingsStatus :: Maybe E'Status2
  
instance Arbitrary PasswordPolicyRecoveryFactors where
  arbitrary = sized genPasswordPolicyRecoveryFactors

genPasswordPolicyRecoveryFactors :: Int -> Gen PasswordPolicyRecoveryFactors
genPasswordPolicyRecoveryFactors n =
  PasswordPolicyRecoveryFactors
    <$> arbitraryReducedMaybe n -- passwordPolicyRecoveryFactorsOktaCall :: Maybe PasswordPolicyRecoveryFactorSettings
    <*> arbitraryReducedMaybe n -- passwordPolicyRecoveryFactorsOktaEmail :: Maybe PasswordPolicyRecoveryEmail
    <*> arbitraryReducedMaybe n -- passwordPolicyRecoveryFactorsOktaSms :: Maybe PasswordPolicyRecoveryFactorSettings
    <*> arbitraryReducedMaybe n -- passwordPolicyRecoveryFactorsRecoveryQuestion :: Maybe PasswordPolicyRecoveryQuestion
  
instance Arbitrary PasswordPolicyRecoveryQuestion where
  arbitrary = sized genPasswordPolicyRecoveryQuestion

genPasswordPolicyRecoveryQuestion :: Int -> Gen PasswordPolicyRecoveryQuestion
genPasswordPolicyRecoveryQuestion n =
  PasswordPolicyRecoveryQuestion
    <$> arbitraryReducedMaybe n -- passwordPolicyRecoveryQuestionProperties :: Maybe PasswordPolicyRecoveryQuestionProperties
    <*> arbitraryReducedMaybe n -- passwordPolicyRecoveryQuestionStatus :: Maybe E'Status2
  
instance Arbitrary PasswordPolicyRecoveryQuestionComplexity where
  arbitrary = sized genPasswordPolicyRecoveryQuestionComplexity

genPasswordPolicyRecoveryQuestionComplexity :: Int -> Gen PasswordPolicyRecoveryQuestionComplexity
genPasswordPolicyRecoveryQuestionComplexity n =
  PasswordPolicyRecoveryQuestionComplexity
    <$> arbitraryReducedMaybe n -- passwordPolicyRecoveryQuestionComplexityMinLength :: Maybe Int
  
instance Arbitrary PasswordPolicyRecoveryQuestionProperties where
  arbitrary = sized genPasswordPolicyRecoveryQuestionProperties

genPasswordPolicyRecoveryQuestionProperties :: Int -> Gen PasswordPolicyRecoveryQuestionProperties
genPasswordPolicyRecoveryQuestionProperties n =
  PasswordPolicyRecoveryQuestionProperties
    <$> arbitraryReducedMaybe n -- passwordPolicyRecoveryQuestionPropertiesComplexity :: Maybe PasswordPolicyRecoveryQuestionComplexity
  
instance Arbitrary PasswordPolicyRecoverySettings where
  arbitrary = sized genPasswordPolicyRecoverySettings

genPasswordPolicyRecoverySettings :: Int -> Gen PasswordPolicyRecoverySettings
genPasswordPolicyRecoverySettings n =
  PasswordPolicyRecoverySettings
    <$> arbitraryReducedMaybe n -- passwordPolicyRecoverySettingsFactors :: Maybe PasswordPolicyRecoveryFactors
  
instance Arbitrary PasswordPolicyRule where
  arbitrary = sized genPasswordPolicyRule

genPasswordPolicyRule :: Int -> Gen PasswordPolicyRule
genPasswordPolicyRule n =
  PasswordPolicyRule
    <$> arbitraryReducedMaybe n -- passwordPolicyRuleActions :: Maybe PasswordPolicyRuleActions
    <*> arbitraryReducedMaybe n -- passwordPolicyRuleConditions :: Maybe PasswordPolicyRuleConditions
    <*> arbitraryReducedMaybe n -- passwordPolicyRuleName :: Maybe Text
  
instance Arbitrary PasswordPolicyRuleAction where
  arbitrary = sized genPasswordPolicyRuleAction

genPasswordPolicyRuleAction :: Int -> Gen PasswordPolicyRuleAction
genPasswordPolicyRuleAction n =
  PasswordPolicyRuleAction
    <$> arbitraryReducedMaybe n -- passwordPolicyRuleActionAccess :: Maybe E'Access
  
instance Arbitrary PasswordPolicyRuleActions where
  arbitrary = sized genPasswordPolicyRuleActions

genPasswordPolicyRuleActions :: Int -> Gen PasswordPolicyRuleActions
genPasswordPolicyRuleActions n =
  PasswordPolicyRuleActions
    <$> arbitraryReducedMaybe n -- passwordPolicyRuleActionsPasswordChange :: Maybe PasswordPolicyRuleAction
    <*> arbitraryReducedMaybe n -- passwordPolicyRuleActionsSelfServicePasswordReset :: Maybe PasswordPolicyRuleAction
    <*> arbitraryReducedMaybe n -- passwordPolicyRuleActionsSelfServiceUnlock :: Maybe PasswordPolicyRuleAction
  
instance Arbitrary PasswordPolicyRuleConditions where
  arbitrary = sized genPasswordPolicyRuleConditions

genPasswordPolicyRuleConditions :: Int -> Gen PasswordPolicyRuleConditions
genPasswordPolicyRuleConditions n =
  PasswordPolicyRuleConditions
    <$> arbitraryReducedMaybe n -- passwordPolicyRuleConditionsNetwork :: Maybe PolicyNetworkCondition
    <*> arbitraryReducedMaybe n -- passwordPolicyRuleConditionsPeople :: Maybe PolicyPeopleCondition
  
instance Arbitrary PasswordPolicySettings where
  arbitrary = sized genPasswordPolicySettings

genPasswordPolicySettings :: Int -> Gen PasswordPolicySettings
genPasswordPolicySettings n =
  PasswordPolicySettings
    <$> arbitraryReducedMaybe n -- passwordPolicySettingsDelegation :: Maybe PasswordPolicyDelegationSettings
    <*> arbitraryReducedMaybe n -- passwordPolicySettingsPassword :: Maybe PasswordPolicyPasswordSettings
    <*> arbitraryReducedMaybe n -- passwordPolicySettingsRecovery :: Maybe PasswordPolicyRecoverySettings
  
instance Arbitrary PasswordSettingObject where
  arbitrary = sized genPasswordSettingObject

genPasswordSettingObject :: Int -> Gen PasswordSettingObject
genPasswordSettingObject n =
  PasswordSettingObject
    <$> arbitraryReducedMaybe n -- passwordSettingObjectChange :: Maybe ChangeEnum
    <*> arbitraryReducedMaybe n -- passwordSettingObjectSeed :: Maybe SeedEnum
    <*> arbitraryReducedMaybe n -- passwordSettingObjectStatus :: Maybe EnabledStatus
  
instance Arbitrary PlatformConditionEvaluatorPlatform where
  arbitrary = sized genPlatformConditionEvaluatorPlatform

genPlatformConditionEvaluatorPlatform :: Int -> Gen PlatformConditionEvaluatorPlatform
genPlatformConditionEvaluatorPlatform n =
  PlatformConditionEvaluatorPlatform
    <$> arbitraryReducedMaybe n -- platformConditionEvaluatorPlatformOs :: Maybe PlatformConditionEvaluatorPlatformOperatingSystem
    <*> arbitraryReducedMaybe n -- platformConditionEvaluatorPlatformType :: Maybe E'Type6
  
instance Arbitrary PlatformConditionEvaluatorPlatformOperatingSystem where
  arbitrary = sized genPlatformConditionEvaluatorPlatformOperatingSystem

genPlatformConditionEvaluatorPlatformOperatingSystem :: Int -> Gen PlatformConditionEvaluatorPlatformOperatingSystem
genPlatformConditionEvaluatorPlatformOperatingSystem n =
  PlatformConditionEvaluatorPlatformOperatingSystem
    <$> arbitraryReducedMaybe n -- platformConditionEvaluatorPlatformOperatingSystemExpression :: Maybe Text
    <*> arbitraryReducedMaybe n -- platformConditionEvaluatorPlatformOperatingSystemType :: Maybe E'Type7
    <*> arbitraryReducedMaybe n -- platformConditionEvaluatorPlatformOperatingSystemVersion :: Maybe PlatformConditionEvaluatorPlatformOperatingSystemVersion
  
instance Arbitrary PlatformConditionEvaluatorPlatformOperatingSystemVersion where
  arbitrary = sized genPlatformConditionEvaluatorPlatformOperatingSystemVersion

genPlatformConditionEvaluatorPlatformOperatingSystemVersion :: Int -> Gen PlatformConditionEvaluatorPlatformOperatingSystemVersion
genPlatformConditionEvaluatorPlatformOperatingSystemVersion n =
  PlatformConditionEvaluatorPlatformOperatingSystemVersion
    <$> arbitraryReducedMaybe n -- platformConditionEvaluatorPlatformOperatingSystemVersionMatchType :: Maybe E'MatchType
    <*> arbitraryReducedMaybe n -- platformConditionEvaluatorPlatformOperatingSystemVersionValue :: Maybe Text
  
instance Arbitrary PlatformPolicyRuleCondition where
  arbitrary = sized genPlatformPolicyRuleCondition

genPlatformPolicyRuleCondition :: Int -> Gen PlatformPolicyRuleCondition
genPlatformPolicyRuleCondition n =
  PlatformPolicyRuleCondition
    <$> arbitraryReducedMaybe n -- platformPolicyRuleConditionExclude :: Maybe [PlatformConditionEvaluatorPlatform]
    <*> arbitraryReducedMaybe n -- platformPolicyRuleConditionInclude :: Maybe [PlatformConditionEvaluatorPlatform]
  
instance Arbitrary Policy where
  arbitrary = sized genPolicy

genPolicy :: Int -> Gen Policy
genPolicy n =
  Policy
    <$> arbitraryReducedMaybe n -- policyEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- policyLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- policyConditions :: Maybe PolicyRuleConditions
    <*> arbitraryReducedMaybe n -- policyCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- policyDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- policyId :: Maybe Text
    <*> arbitraryReducedMaybe n -- policyLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- policyName :: Maybe Text
    <*> arbitraryReducedMaybe n -- policyPriority :: Maybe Int
    <*> arbitraryReducedMaybe n -- policyStatus :: Maybe E'Status2
    <*> arbitraryReducedMaybe n -- policySystem :: Maybe Bool
    <*> arbitraryReducedMaybe n -- policyType :: Maybe PolicyType
  
instance Arbitrary PolicyAccountLink where
  arbitrary = sized genPolicyAccountLink

genPolicyAccountLink :: Int -> Gen PolicyAccountLink
genPolicyAccountLink n =
  PolicyAccountLink
    <$> arbitraryReducedMaybe n -- policyAccountLinkAction :: Maybe E'Action
    <*> arbitraryReducedMaybe n -- policyAccountLinkFilter :: Maybe PolicyAccountLinkFilter
  
instance Arbitrary PolicyAccountLinkFilter where
  arbitrary = sized genPolicyAccountLinkFilter

genPolicyAccountLinkFilter :: Int -> Gen PolicyAccountLinkFilter
genPolicyAccountLinkFilter n =
  PolicyAccountLinkFilter
    <$> arbitraryReducedMaybe n -- policyAccountLinkFilterGroups :: Maybe PolicyAccountLinkFilterGroups
  
instance Arbitrary PolicyAccountLinkFilterGroups where
  arbitrary = sized genPolicyAccountLinkFilterGroups

genPolicyAccountLinkFilterGroups :: Int -> Gen PolicyAccountLinkFilterGroups
genPolicyAccountLinkFilterGroups n =
  PolicyAccountLinkFilterGroups
    <$> arbitraryReducedMaybe n -- policyAccountLinkFilterGroupsInclude :: Maybe [Text]
  
instance Arbitrary PolicyNetworkCondition where
  arbitrary = sized genPolicyNetworkCondition

genPolicyNetworkCondition :: Int -> Gen PolicyNetworkCondition
genPolicyNetworkCondition n =
  PolicyNetworkCondition
    <$> arbitraryReducedMaybe n -- policyNetworkConditionConnection :: Maybe E'Connection
    <*> arbitraryReducedMaybe n -- policyNetworkConditionExclude :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- policyNetworkConditionInclude :: Maybe [Text]
  
instance Arbitrary PolicyPeopleCondition where
  arbitrary = sized genPolicyPeopleCondition

genPolicyPeopleCondition :: Int -> Gen PolicyPeopleCondition
genPolicyPeopleCondition n =
  PolicyPeopleCondition
    <$> arbitraryReducedMaybe n -- policyPeopleConditionGroups :: Maybe GroupCondition
    <*> arbitraryReducedMaybe n -- policyPeopleConditionUsers :: Maybe UserCondition
  
instance Arbitrary PolicyRule where
  arbitrary = sized genPolicyRule

genPolicyRule :: Int -> Gen PolicyRule
genPolicyRule n =
  PolicyRule
    <$> arbitraryReducedMaybe n -- policyRuleCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- policyRuleId :: Maybe Text
    <*> arbitraryReducedMaybe n -- policyRuleLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- policyRulePriority :: Maybe Int
    <*> arbitraryReducedMaybe n -- policyRuleStatus :: Maybe E'Status2
    <*> arbitraryReducedMaybe n -- policyRuleSystem :: Maybe Bool
    <*> arbitraryReducedMaybe n -- policyRuleType :: Maybe E'Type8
    <*> arbitraryReducedMaybe n -- policyRuleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- policyRuleConditions :: Maybe PolicyRuleConditions
    <*> arbitraryReducedMaybe n -- policyRuleActions :: Maybe PolicyRuleActions
  
instance Arbitrary PolicyRuleActions where
  arbitrary = sized genPolicyRuleActions

genPolicyRuleActions :: Int -> Gen PolicyRuleActions
genPolicyRuleActions n =
  PolicyRuleActions
    <$> arbitraryReducedMaybe n -- policyRuleActionsEnroll :: Maybe PolicyRuleActionsEnroll
    <*> arbitraryReducedMaybe n -- policyRuleActionsSignon :: Maybe OktaSignOnPolicyRuleSignonActions
    <*> arbitraryReducedMaybe n -- policyRuleActionsPasswordChange :: Maybe PasswordPolicyRuleAction
    <*> arbitraryReducedMaybe n -- policyRuleActionsSelfServicePasswordReset :: Maybe PasswordPolicyRuleAction
    <*> arbitraryReducedMaybe n -- policyRuleActionsSelfServiceUnlock :: Maybe PasswordPolicyRuleAction
    <*> arbitraryReducedMaybe n -- policyRuleActionsIdp :: Maybe IdpPolicyRuleAction
  
instance Arbitrary PolicyRuleActionsEnroll where
  arbitrary = sized genPolicyRuleActionsEnroll

genPolicyRuleActionsEnroll :: Int -> Gen PolicyRuleActionsEnroll
genPolicyRuleActionsEnroll n =
  PolicyRuleActionsEnroll
    <$> arbitraryReducedMaybe n -- policyRuleActionsEnrollSelf :: Maybe PolicyRuleActionsEnrollSelf
  
instance Arbitrary PolicyRuleAuthContextCondition where
  arbitrary = sized genPolicyRuleAuthContextCondition

genPolicyRuleAuthContextCondition :: Int -> Gen PolicyRuleAuthContextCondition
genPolicyRuleAuthContextCondition n =
  PolicyRuleAuthContextCondition
    <$> arbitraryReducedMaybe n -- policyRuleAuthContextConditionAuthType :: Maybe E'AuthType
  
instance Arbitrary PolicyRuleConditions where
  arbitrary = sized genPolicyRuleConditions

genPolicyRuleConditions :: Int -> Gen PolicyRuleConditions
genPolicyRuleConditions n =
  PolicyRuleConditions
    <$> arbitraryReducedMaybe n -- policyRuleConditionsApp :: Maybe AppAndInstancePolicyRuleCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsApps :: Maybe AppInstancePolicyRuleCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsAuthContext :: Maybe PolicyRuleAuthContextCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsAuthProvider :: Maybe PasswordPolicyAuthenticationProviderCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsBeforeScheduledAction :: Maybe BeforeScheduledActionPolicyRuleCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsClients :: Maybe ClientPolicyCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsContext :: Maybe ContextPolicyRuleCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsDevice :: Maybe DevicePolicyRuleCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsGrantTypes :: Maybe GrantTypePolicyRuleCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsGroups :: Maybe GroupPolicyRuleCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsIdentityProvider :: Maybe IdentityProviderPolicyRuleCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsMdmEnrollment :: Maybe MDMEnrollmentPolicyRuleCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsNetwork :: Maybe PolicyNetworkCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsPeople :: Maybe PolicyPeopleCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsPlatform :: Maybe PlatformPolicyRuleCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsRisk :: Maybe RiskPolicyRuleCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsRiskScore :: Maybe RiskScorePolicyRuleCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsScopes :: Maybe OAuth2ScopesMediationPolicyRuleCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsUserIdentifier :: Maybe UserIdentifierPolicyRuleCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsUserStatus :: Maybe UserStatusPolicyRuleCondition
    <*> arbitraryReducedMaybe n -- policyRuleConditionsUsers :: Maybe UserPolicyRuleCondition
  
instance Arbitrary PolicySubject where
  arbitrary = sized genPolicySubject

genPolicySubject :: Int -> Gen PolicySubject
genPolicySubject n =
  PolicySubject
    <$> arbitraryReducedMaybe n -- policySubjectFilter :: Maybe Text
    <*> arbitraryReducedMaybe n -- policySubjectFormat :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- policySubjectMatchAttribute :: Maybe Text
    <*> arbitraryReducedMaybe n -- policySubjectMatchType :: Maybe PolicySubjectMatchType
    <*> arbitraryReducedMaybe n -- policySubjectUserNameTemplate :: Maybe PolicyUserNameTemplate
  
instance Arbitrary PolicyUserNameTemplate where
  arbitrary = sized genPolicyUserNameTemplate

genPolicyUserNameTemplate :: Int -> Gen PolicyUserNameTemplate
genPolicyUserNameTemplate n =
  PolicyUserNameTemplate
    <$> arbitraryReducedMaybe n -- policyUserNameTemplateTemplate :: Maybe Text
  
instance Arbitrary PossessionConstraint where
  arbitrary = sized genPossessionConstraint

genPossessionConstraint :: Int -> Gen PossessionConstraint
genPossessionConstraint n =
  PossessionConstraint
    <$> arbitraryReducedMaybe n -- possessionConstraintHardwareProtection :: Maybe Text
    <*> arbitraryReducedMaybe n -- possessionConstraintDeviceBound :: Maybe Text
    <*> arbitraryReducedMaybe n -- possessionConstraintPhishingResistant :: Maybe Text
    <*> arbitraryReducedMaybe n -- possessionConstraintUserPresence :: Maybe Text
  
instance Arbitrary PreRegistrationInlineHook where
  arbitrary = sized genPreRegistrationInlineHook

genPreRegistrationInlineHook :: Int -> Gen PreRegistrationInlineHook
genPreRegistrationInlineHook n =
  PreRegistrationInlineHook
    <$> arbitraryReducedMaybe n -- preRegistrationInlineHookInlineHookId :: Maybe Text
  
instance Arbitrary PreviewSAMLAppMetadata200Response where
  arbitrary = sized genPreviewSAMLAppMetadata200Response

genPreviewSAMLAppMetadata200Response :: Int -> Gen PreviewSAMLAppMetadata200Response
genPreviewSAMLAppMetadata200Response n =
  PreviewSAMLAppMetadata200Response
    <$> arbitraryReducedMaybe n -- previewSAMLAppMetadata200ResponseEntityDescriptor :: Maybe PreviewSAMLAppMetadata200ResponseEntityDescriptor
  
instance Arbitrary PreviewSAMLAppMetadata200ResponseEntityDescriptor where
  arbitrary = sized genPreviewSAMLAppMetadata200ResponseEntityDescriptor

genPreviewSAMLAppMetadata200ResponseEntityDescriptor :: Int -> Gen PreviewSAMLAppMetadata200ResponseEntityDescriptor
genPreviewSAMLAppMetadata200ResponseEntityDescriptor n =
  PreviewSAMLAppMetadata200ResponseEntityDescriptor
    <$> arbitraryReducedMaybe n -- previewSAMLAppMetadata200ResponseEntityDescriptorEntityId :: Maybe Text
    <*> arbitraryReducedMaybe n -- previewSAMLAppMetadata200ResponseEntityDescriptorIdpssoDescriptor :: Maybe PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptor
  
instance Arbitrary PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptor where
  arbitrary = sized genPreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptor

genPreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptor :: Int -> Gen PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptor
genPreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptor n =
  PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptor
    <$> arbitraryReducedMaybe n -- previewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorWantAuthnRequestsSigned :: Maybe Bool
    <*> arbitraryReducedMaybe n -- previewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorProtocolSupportEnumeration :: Maybe Text
    <*> arbitraryReducedMaybe n -- previewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptor :: Maybe PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptor
    <*> arbitraryReducedMaybe n -- previewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorNameIdFormat :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- previewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorSingleSignOnService :: Maybe PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorSingleSignOnService
    <*> arbitraryReducedMaybe n -- previewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorSingleLogoutService :: Maybe PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorSingleSignOnService
  
instance Arbitrary PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptor where
  arbitrary = sized genPreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptor

genPreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptor :: Int -> Gen PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptor
genPreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptor n =
  PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptor
    <$> arbitraryReducedMaybe n -- previewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptorUse :: Maybe Text
    <*> arbitraryReducedMaybe n -- previewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptorKeyInfo :: Maybe PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptorKeyInfo
  
instance Arbitrary PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptorKeyInfo where
  arbitrary = sized genPreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptorKeyInfo

genPreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptorKeyInfo :: Int -> Gen PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptorKeyInfo
genPreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptorKeyInfo n =
  PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptorKeyInfo
    <$> arbitraryReducedMaybe n -- previewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptorKeyInfoX509Data :: Maybe PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptorKeyInfoX509Data
  
instance Arbitrary PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptorKeyInfoX509Data where
  arbitrary = sized genPreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptorKeyInfoX509Data

genPreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptorKeyInfoX509Data :: Int -> Gen PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptorKeyInfoX509Data
genPreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptorKeyInfoX509Data n =
  PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptorKeyInfoX509Data
    <$> arbitraryReducedMaybe n -- previewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorKeyDescriptorKeyInfoX509DataX509Certificate :: Maybe Text
  
instance Arbitrary PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorSingleSignOnService where
  arbitrary = sized genPreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorSingleSignOnService

genPreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorSingleSignOnService :: Int -> Gen PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorSingleSignOnService
genPreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorSingleSignOnService n =
  PreviewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorSingleSignOnService
    <$> arbitraryReducedMaybe n -- previewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorSingleSignOnServiceBinding :: Maybe Text
    <*> arbitraryReducedMaybe n -- previewSAMLAppMetadata200ResponseEntityDescriptorIDPSSODescriptorSingleSignOnServiceLocation :: Maybe Text
  
instance Arbitrary ProfileEnrollmentPolicyRule where
  arbitrary = sized genProfileEnrollmentPolicyRule

genProfileEnrollmentPolicyRule :: Int -> Gen ProfileEnrollmentPolicyRule
genProfileEnrollmentPolicyRule n =
  ProfileEnrollmentPolicyRule
    <$> arbitraryReducedMaybe n -- profileEnrollmentPolicyRuleActions :: Maybe ProfileEnrollmentPolicyRuleActions
    <*> arbitraryReducedMaybe n -- profileEnrollmentPolicyRuleName :: Maybe Text
  
instance Arbitrary ProfileEnrollmentPolicyRuleAction where
  arbitrary = sized genProfileEnrollmentPolicyRuleAction

genProfileEnrollmentPolicyRuleAction :: Int -> Gen ProfileEnrollmentPolicyRuleAction
genProfileEnrollmentPolicyRuleAction n =
  ProfileEnrollmentPolicyRuleAction
    <$> arbitraryReducedMaybe n -- profileEnrollmentPolicyRuleActionAccess :: Maybe Text
    <*> arbitraryReducedMaybe n -- profileEnrollmentPolicyRuleActionPreRegistrationInlineHooks :: Maybe [PreRegistrationInlineHook]
    <*> arbitraryReducedMaybe n -- profileEnrollmentPolicyRuleActionProfileAttributes :: Maybe [ProfileEnrollmentPolicyRuleProfileAttribute]
    <*> arbitraryReducedMaybe n -- profileEnrollmentPolicyRuleActionActivationRequirements :: Maybe ProfileEnrollmentPolicyRuleActivationRequirement
    <*> arbitraryReducedMaybe n -- profileEnrollmentPolicyRuleActionTargetGroupIds :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- profileEnrollmentPolicyRuleActionUnknownUserAction :: Maybe Text
    <*> arbitraryReducedMaybe n -- profileEnrollmentPolicyRuleActionUiSchemaId :: Maybe Text
  
instance Arbitrary ProfileEnrollmentPolicyRuleActions where
  arbitrary = sized genProfileEnrollmentPolicyRuleActions

genProfileEnrollmentPolicyRuleActions :: Int -> Gen ProfileEnrollmentPolicyRuleActions
genProfileEnrollmentPolicyRuleActions n =
  ProfileEnrollmentPolicyRuleActions
    <$> arbitraryReducedMaybe n -- profileEnrollmentPolicyRuleActionsProfileEnrollment :: Maybe ProfileEnrollmentPolicyRuleAction
  
instance Arbitrary ProfileEnrollmentPolicyRuleActivationRequirement where
  arbitrary = sized genProfileEnrollmentPolicyRuleActivationRequirement

genProfileEnrollmentPolicyRuleActivationRequirement :: Int -> Gen ProfileEnrollmentPolicyRuleActivationRequirement
genProfileEnrollmentPolicyRuleActivationRequirement n =
  ProfileEnrollmentPolicyRuleActivationRequirement
    <$> arbitraryReducedMaybe n -- profileEnrollmentPolicyRuleActivationRequirementEmailVerification :: Maybe Bool
  
instance Arbitrary ProfileEnrollmentPolicyRuleProfileAttribute where
  arbitrary = sized genProfileEnrollmentPolicyRuleProfileAttribute

genProfileEnrollmentPolicyRuleProfileAttribute :: Int -> Gen ProfileEnrollmentPolicyRuleProfileAttribute
genProfileEnrollmentPolicyRuleProfileAttribute n =
  ProfileEnrollmentPolicyRuleProfileAttribute
    <$> arbitraryReducedMaybe n -- profileEnrollmentPolicyRuleProfileAttributeLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- profileEnrollmentPolicyRuleProfileAttributeName :: Maybe Text
    <*> arbitraryReducedMaybe n -- profileEnrollmentPolicyRuleProfileAttributeRequired :: Maybe Bool
  
instance Arbitrary ProfileMapping where
  arbitrary = sized genProfileMapping

genProfileMapping :: Int -> Gen ProfileMapping
genProfileMapping n =
  ProfileMapping
    <$> arbitraryReducedMaybe n -- profileMappingLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- profileMappingId :: Maybe Text
    <*> arbitraryReducedMaybe n -- profileMappingProperties :: Maybe (Map.Map String ProfileMappingProperty)
    <*> arbitraryReducedMaybe n -- profileMappingSource :: Maybe ProfileMappingSource
    <*> arbitraryReducedMaybe n -- profileMappingTarget :: Maybe ProfileMappingSource
  
instance Arbitrary ProfileMappingProperty where
  arbitrary = sized genProfileMappingProperty

genProfileMappingProperty :: Int -> Gen ProfileMappingProperty
genProfileMappingProperty n =
  ProfileMappingProperty
    <$> arbitraryReducedMaybe n -- profileMappingPropertyExpression :: Maybe Text
    <*> arbitraryReducedMaybe n -- profileMappingPropertyPushStatus :: Maybe ProfileMappingPropertyPushStatus
  
instance Arbitrary ProfileMappingPropertyPushStatus where
  arbitrary = sized genProfileMappingPropertyPushStatus

genProfileMappingPropertyPushStatus :: Int -> Gen ProfileMappingPropertyPushStatus
genProfileMappingPropertyPushStatus n =
  
  pure ProfileMappingPropertyPushStatus
   
instance Arbitrary ProfileMappingSource where
  arbitrary = sized genProfileMappingSource

genProfileMappingSource :: Int -> Gen ProfileMappingSource
genProfileMappingSource n =
  ProfileMappingSource
    <$> arbitraryReducedMaybe n -- profileMappingSourceLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- profileMappingSourceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- profileMappingSourceName :: Maybe Text
    <*> arbitraryReducedMaybe n -- profileMappingSourceType :: Maybe Text
  
instance Arbitrary ProfileSettingObject where
  arbitrary = sized genProfileSettingObject

genProfileSettingObject :: Int -> Gen ProfileSettingObject
genProfileSettingObject n =
  ProfileSettingObject
    <$> arbitraryReducedMaybe n -- profileSettingObjectStatus :: Maybe EnabledStatus
  
instance Arbitrary Protocol where
  arbitrary = sized genProtocol

genProtocol :: Int -> Gen Protocol
genProtocol n =
  Protocol
    <$> arbitraryReducedMaybe n -- protocolAlgorithms :: Maybe ProtocolAlgorithms
    <*> arbitraryReducedMaybe n -- protocolCredentials :: Maybe IdentityProviderCredentials
    <*> arbitraryReducedMaybe n -- protocolEndpoints :: Maybe ProtocolEndpoints
    <*> arbitraryReducedMaybe n -- protocolIssuer :: Maybe ProtocolEndpoint
    <*> arbitraryReducedMaybe n -- protocolRelayState :: Maybe ProtocolRelayState
    <*> arbitraryReducedMaybe n -- protocolScopes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- protocolSettings :: Maybe ProtocolSettings
    <*> arbitraryReducedMaybe n -- protocolType :: Maybe E'Type9
  
instance Arbitrary ProtocolAlgorithmType where
  arbitrary = sized genProtocolAlgorithmType

genProtocolAlgorithmType :: Int -> Gen ProtocolAlgorithmType
genProtocolAlgorithmType n =
  ProtocolAlgorithmType
    <$> arbitraryReducedMaybe n -- protocolAlgorithmTypeSignature :: Maybe ProtocolAlgorithmTypeSignature
  
instance Arbitrary ProtocolAlgorithmTypeSignature where
  arbitrary = sized genProtocolAlgorithmTypeSignature

genProtocolAlgorithmTypeSignature :: Int -> Gen ProtocolAlgorithmTypeSignature
genProtocolAlgorithmTypeSignature n =
  ProtocolAlgorithmTypeSignature
    <$> arbitraryReducedMaybe n -- protocolAlgorithmTypeSignatureAlgorithm :: Maybe Text
    <*> arbitraryReducedMaybe n -- protocolAlgorithmTypeSignatureScope :: Maybe E'Scope
  
instance Arbitrary ProtocolAlgorithms where
  arbitrary = sized genProtocolAlgorithms

genProtocolAlgorithms :: Int -> Gen ProtocolAlgorithms
genProtocolAlgorithms n =
  ProtocolAlgorithms
    <$> arbitraryReducedMaybe n -- protocolAlgorithmsRequest :: Maybe ProtocolAlgorithmType
    <*> arbitraryReducedMaybe n -- protocolAlgorithmsResponse :: Maybe ProtocolAlgorithmType
  
instance Arbitrary ProtocolEndpoint where
  arbitrary = sized genProtocolEndpoint

genProtocolEndpoint :: Int -> Gen ProtocolEndpoint
genProtocolEndpoint n =
  ProtocolEndpoint
    <$> arbitraryReducedMaybe n -- protocolEndpointBinding :: Maybe E'Binding
    <*> arbitraryReducedMaybe n -- protocolEndpointDestination :: Maybe Text
    <*> arbitraryReducedMaybe n -- protocolEndpointType :: Maybe E'Type10
    <*> arbitraryReducedMaybe n -- protocolEndpointUrl :: Maybe Text
  
instance Arbitrary ProtocolEndpoints where
  arbitrary = sized genProtocolEndpoints

genProtocolEndpoints :: Int -> Gen ProtocolEndpoints
genProtocolEndpoints n =
  ProtocolEndpoints
    <$> arbitraryReducedMaybe n -- protocolEndpointsAcs :: Maybe ProtocolEndpoint
    <*> arbitraryReducedMaybe n -- protocolEndpointsAuthorization :: Maybe ProtocolEndpoint
    <*> arbitraryReducedMaybe n -- protocolEndpointsJwks :: Maybe ProtocolEndpoint
    <*> arbitraryReducedMaybe n -- protocolEndpointsMetadata :: Maybe ProtocolEndpoint
    <*> arbitraryReducedMaybe n -- protocolEndpointsSlo :: Maybe ProtocolEndpoint
    <*> arbitraryReducedMaybe n -- protocolEndpointsSso :: Maybe ProtocolEndpoint
    <*> arbitraryReducedMaybe n -- protocolEndpointsToken :: Maybe ProtocolEndpoint
    <*> arbitraryReducedMaybe n -- protocolEndpointsUserInfo :: Maybe ProtocolEndpoint
  
instance Arbitrary ProtocolRelayState where
  arbitrary = sized genProtocolRelayState

genProtocolRelayState :: Int -> Gen ProtocolRelayState
genProtocolRelayState n =
  ProtocolRelayState
    <$> arbitraryReducedMaybe n -- protocolRelayStateFormat :: Maybe ProtocolRelayStateFormat
  
instance Arbitrary ProtocolSettings where
  arbitrary = sized genProtocolSettings

genProtocolSettings :: Int -> Gen ProtocolSettings
genProtocolSettings n =
  ProtocolSettings
    <$> arbitraryReducedMaybe n -- protocolSettingsNameFormat :: Maybe Text
  
instance Arbitrary Provisioning where
  arbitrary = sized genProvisioning

genProvisioning :: Int -> Gen Provisioning
genProvisioning n =
  Provisioning
    <$> arbitraryReducedMaybe n -- provisioningAction :: Maybe E'Action2
    <*> arbitraryReducedMaybe n -- provisioningConditions :: Maybe ProvisioningConditions
    <*> arbitraryReducedMaybe n -- provisioningGroups :: Maybe ProvisioningGroups
    <*> arbitraryReducedMaybe n -- provisioningProfileMaster :: Maybe Bool
  
instance Arbitrary ProvisioningConditions where
  arbitrary = sized genProvisioningConditions

genProvisioningConditions :: Int -> Gen ProvisioningConditions
genProvisioningConditions n =
  ProvisioningConditions
    <$> arbitraryReducedMaybe n -- provisioningConditionsDeprovisioned :: Maybe ProvisioningDeprovisionedCondition
    <*> arbitraryReducedMaybe n -- provisioningConditionsSuspended :: Maybe ProvisioningSuspendedCondition
  
instance Arbitrary ProvisioningConnection where
  arbitrary = sized genProvisioningConnection

genProvisioningConnection :: Int -> Gen ProvisioningConnection
genProvisioningConnection n =
  ProvisioningConnection
    <$> arbitraryReducedMaybe n -- provisioningConnectionAuthScheme :: Maybe ProvisioningConnectionAuthScheme
    <*> arbitraryReducedMaybe n -- provisioningConnectionStatus :: Maybe ProvisioningConnectionStatus
    <*> arbitraryReducedMaybe n -- provisioningConnectionLinks :: Maybe (Map.Map String A.Value)
  
instance Arbitrary ProvisioningConnectionProfile where
  arbitrary = sized genProvisioningConnectionProfile

genProvisioningConnectionProfile :: Int -> Gen ProvisioningConnectionProfile
genProvisioningConnectionProfile n =
  ProvisioningConnectionProfile
    <$> arbitraryReducedMaybe n -- provisioningConnectionProfileAuthScheme :: Maybe ProvisioningConnectionAuthScheme
    <*> arbitraryReducedMaybe n -- provisioningConnectionProfileToken :: Maybe Text
  
instance Arbitrary ProvisioningConnectionRequest where
  arbitrary = sized genProvisioningConnectionRequest

genProvisioningConnectionRequest :: Int -> Gen ProvisioningConnectionRequest
genProvisioningConnectionRequest n =
  ProvisioningConnectionRequest
    <$> arbitraryReducedMaybe n -- provisioningConnectionRequestProfile :: Maybe ProvisioningConnectionProfile
  
instance Arbitrary ProvisioningDeprovisionedCondition where
  arbitrary = sized genProvisioningDeprovisionedCondition

genProvisioningDeprovisionedCondition :: Int -> Gen ProvisioningDeprovisionedCondition
genProvisioningDeprovisionedCondition n =
  ProvisioningDeprovisionedCondition
    <$> arbitraryReducedMaybe n -- provisioningDeprovisionedConditionAction :: Maybe E'Action3
  
instance Arbitrary ProvisioningGroups where
  arbitrary = sized genProvisioningGroups

genProvisioningGroups :: Int -> Gen ProvisioningGroups
genProvisioningGroups n =
  ProvisioningGroups
    <$> arbitraryReducedMaybe n -- provisioningGroupsAction :: Maybe E'Action4
    <*> arbitraryReducedMaybe n -- provisioningGroupsAssignments :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- provisioningGroupsFilter :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- provisioningGroupsSourceAttributeName :: Maybe Text
  
instance Arbitrary ProvisioningSuspendedCondition where
  arbitrary = sized genProvisioningSuspendedCondition

genProvisioningSuspendedCondition :: Int -> Gen ProvisioningSuspendedCondition
genProvisioningSuspendedCondition n =
  ProvisioningSuspendedCondition
    <$> arbitraryReducedMaybe n -- provisioningSuspendedConditionAction :: Maybe E'Action5
  
instance Arbitrary PushUserFactor where
  arbitrary = sized genPushUserFactor

genPushUserFactor :: Int -> Gen PushUserFactor
genPushUserFactor n =
  PushUserFactor
    <$> arbitraryReducedMaybe n -- pushUserFactorExpiresAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- pushUserFactorFactorResult :: Maybe FactorResultType
    <*> arbitraryReducedMaybe n -- pushUserFactorProfile :: Maybe PushUserFactorProfile
  
instance Arbitrary PushUserFactorProfile where
  arbitrary = sized genPushUserFactorProfile

genPushUserFactorProfile :: Int -> Gen PushUserFactorProfile
genPushUserFactorProfile n =
  PushUserFactorProfile
    <$> arbitraryReducedMaybe n -- pushUserFactorProfileCredentialId :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushUserFactorProfileDeviceToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushUserFactorProfileDeviceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushUserFactorProfileName :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushUserFactorProfilePlatform :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushUserFactorProfileVersion :: Maybe Text
  
instance Arbitrary RecoveryQuestionCredential where
  arbitrary = sized genRecoveryQuestionCredential

genRecoveryQuestionCredential :: Int -> Gen RecoveryQuestionCredential
genRecoveryQuestionCredential n =
  RecoveryQuestionCredential
    <$> arbitraryReducedMaybe n -- recoveryQuestionCredentialAnswer :: Maybe Text
    <*> arbitraryReducedMaybe n -- recoveryQuestionCredentialQuestion :: Maybe Text
  
instance Arbitrary ResetPasswordToken where
  arbitrary = sized genResetPasswordToken

genResetPasswordToken :: Int -> Gen ResetPasswordToken
genResetPasswordToken n =
  ResetPasswordToken
    <$> arbitraryReducedMaybe n -- resetPasswordTokenResetPasswordUrl :: Maybe Text
  
instance Arbitrary ResponseLinks where
  arbitrary = sized genResponseLinks

genResponseLinks :: Int -> Gen ResponseLinks
genResponseLinks n =
  ResponseLinks
    <$> arbitraryReducedMaybe n -- responseLinksLinks :: Maybe (Map.Map String A.Value)
  
instance Arbitrary RiskPolicyRuleCondition where
  arbitrary = sized genRiskPolicyRuleCondition

genRiskPolicyRuleCondition :: Int -> Gen RiskPolicyRuleCondition
genRiskPolicyRuleCondition n =
  RiskPolicyRuleCondition
    <$> arbitraryReducedMaybe n -- riskPolicyRuleConditionBehaviors :: Maybe [Text]
  
instance Arbitrary RiskScorePolicyRuleCondition where
  arbitrary = sized genRiskScorePolicyRuleCondition

genRiskScorePolicyRuleCondition :: Int -> Gen RiskScorePolicyRuleCondition
genRiskScorePolicyRuleCondition n =
  RiskScorePolicyRuleCondition
    <$> arbitraryReducedMaybe n -- riskScorePolicyRuleConditionLevel :: Maybe Text
  
instance Arbitrary Role where
  arbitrary = sized genRole

genRole :: Int -> Gen Role
genRole n =
  Role
    <$> arbitraryReducedMaybe n -- roleEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- roleLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- roleAssignmentType :: Maybe RoleAssignmentType
    <*> arbitraryReducedMaybe n -- roleCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- roleDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- roleId :: Maybe Text
    <*> arbitraryReducedMaybe n -- roleLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- roleLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- roleStatus :: Maybe RoleStatus
    <*> arbitraryReducedMaybe n -- roleType :: Maybe RoleType
  
instance Arbitrary SamlApplication where
  arbitrary = sized genSamlApplication

genSamlApplication :: Int -> Gen SamlApplication
genSamlApplication n =
  SamlApplication
    <$> arbitraryReducedMaybe n -- samlApplicationSettings :: Maybe SamlApplicationSettings
  
instance Arbitrary SamlApplicationSettings where
  arbitrary = sized genSamlApplicationSettings

genSamlApplicationSettings :: Int -> Gen SamlApplicationSettings
genSamlApplicationSettings n =
  SamlApplicationSettings
    <$> arbitraryReducedMaybe n -- samlApplicationSettingsSignOn :: Maybe SamlApplicationSettingsSignOn
  
instance Arbitrary SamlApplicationSettingsSignOn where
  arbitrary = sized genSamlApplicationSettingsSignOn

genSamlApplicationSettingsSignOn :: Int -> Gen SamlApplicationSettingsSignOn
genSamlApplicationSettingsSignOn n =
  SamlApplicationSettingsSignOn
    <$> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnAcsEndpoints :: Maybe [AcsEndpoint]
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnAllowMultipleAcsEndpoints :: Maybe Bool
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnAssertionSigned :: Maybe Bool
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnAttributeStatements :: Maybe [SamlAttributeStatement]
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnAudience :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnAudienceOverride :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnAuthnContextClassRef :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnDefaultRelayState :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnDestination :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnDestinationOverride :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnDigestAlgorithm :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnHonorForceAuthn :: Maybe Bool
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnIdpIssuer :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnInlineHooks :: Maybe [SignOnInlineHook]
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnRecipient :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnRecipientOverride :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnRequestCompressed :: Maybe Bool
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnResponseSigned :: Maybe Bool
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnSamlSignedRequestEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnSignatureAlgorithm :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnSlo :: Maybe SingleLogout
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnSpIssuer :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnSpCertificate :: Maybe SpCertificate
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnSsoAcsUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnSsoAcsUrlOverride :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnSubjectNameIdFormat :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnSubjectNameIdTemplate :: Maybe Text
  
instance Arbitrary SamlAttributeStatement where
  arbitrary = sized genSamlAttributeStatement

genSamlAttributeStatement :: Int -> Gen SamlAttributeStatement
genSamlAttributeStatement n =
  SamlAttributeStatement
    <$> arbitraryReducedMaybe n -- samlAttributeStatementName :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlAttributeStatementNamespace :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlAttributeStatementType :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlAttributeStatementFilterType :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlAttributeStatementFilterValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlAttributeStatementValues :: Maybe [Text]
  
instance Arbitrary ScheduledUserLifecycleAction where
  arbitrary = sized genScheduledUserLifecycleAction

genScheduledUserLifecycleAction :: Int -> Gen ScheduledUserLifecycleAction
genScheduledUserLifecycleAction n =
  ScheduledUserLifecycleAction
    <$> arbitraryReducedMaybe n -- scheduledUserLifecycleActionStatus :: Maybe E'Status4
  
instance Arbitrary SchemeApplicationCredentials where
  arbitrary = sized genSchemeApplicationCredentials

genSchemeApplicationCredentials :: Int -> Gen SchemeApplicationCredentials
genSchemeApplicationCredentials n =
  SchemeApplicationCredentials
    <$> arbitraryReducedMaybe n -- schemeApplicationCredentialsPassword :: Maybe PasswordCredential
    <*> arbitraryReducedMaybe n -- schemeApplicationCredentialsRevealPassword :: Maybe Bool
    <*> arbitraryReducedMaybe n -- schemeApplicationCredentialsScheme :: Maybe ApplicationCredentialsScheme
    <*> arbitraryReducedMaybe n -- schemeApplicationCredentialsSigning :: Maybe ApplicationCredentialsSigning
    <*> arbitraryReducedMaybe n -- schemeApplicationCredentialsUserName :: Maybe Text
  
instance Arbitrary Scope where
  arbitrary = sized genScope

genScope :: Int -> Gen Scope
genScope n =
  Scope
    <$> arbitraryReducedMaybe n -- scopeStringValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- scopeType :: Maybe ScopeType
    <*> arbitraryReducedMaybe n -- scopeAllowedOktaApps :: Maybe [IframeEmbedScopeAllowedApps]
  
instance Arbitrary SecurePasswordStoreApplication where
  arbitrary = sized genSecurePasswordStoreApplication

genSecurePasswordStoreApplication :: Int -> Gen SecurePasswordStoreApplication
genSecurePasswordStoreApplication n =
  SecurePasswordStoreApplication
    <$> arbitraryReducedMaybe n -- securePasswordStoreApplicationCredentials :: Maybe SchemeApplicationCredentials
    <*> arbitraryReducedMaybeValue n -- securePasswordStoreApplicationName :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- securePasswordStoreApplicationSettings :: Maybe SecurePasswordStoreApplicationSettings
  
instance Arbitrary SecurePasswordStoreApplicationSettings where
  arbitrary = sized genSecurePasswordStoreApplicationSettings

genSecurePasswordStoreApplicationSettings :: Int -> Gen SecurePasswordStoreApplicationSettings
genSecurePasswordStoreApplicationSettings n =
  SecurePasswordStoreApplicationSettings
    <$> arbitraryReducedMaybe n -- securePasswordStoreApplicationSettingsApp :: Maybe SecurePasswordStoreApplicationSettingsApplication
  
instance Arbitrary SecurePasswordStoreApplicationSettingsApplication where
  arbitrary = sized genSecurePasswordStoreApplicationSettingsApplication

genSecurePasswordStoreApplicationSettingsApplication :: Int -> Gen SecurePasswordStoreApplicationSettingsApplication
genSecurePasswordStoreApplicationSettingsApplication n =
  SecurePasswordStoreApplicationSettingsApplication
    <$> arbitraryReducedMaybe n -- securePasswordStoreApplicationSettingsApplicationOptionalField1 :: Maybe Text
    <*> arbitraryReducedMaybe n -- securePasswordStoreApplicationSettingsApplicationOptionalField1Value :: Maybe Text
    <*> arbitraryReducedMaybe n -- securePasswordStoreApplicationSettingsApplicationOptionalField2 :: Maybe Text
    <*> arbitraryReducedMaybe n -- securePasswordStoreApplicationSettingsApplicationOptionalField2Value :: Maybe Text
    <*> arbitraryReducedMaybe n -- securePasswordStoreApplicationSettingsApplicationOptionalField3 :: Maybe Text
    <*> arbitraryReducedMaybe n -- securePasswordStoreApplicationSettingsApplicationOptionalField3Value :: Maybe Text
    <*> arbitraryReducedMaybe n -- securePasswordStoreApplicationSettingsApplicationPasswordField :: Maybe Text
    <*> arbitraryReducedMaybe n -- securePasswordStoreApplicationSettingsApplicationUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- securePasswordStoreApplicationSettingsApplicationUsernameField :: Maybe Text
  
instance Arbitrary SecurityQuestion where
  arbitrary = sized genSecurityQuestion

genSecurityQuestion :: Int -> Gen SecurityQuestion
genSecurityQuestion n =
  SecurityQuestion
    <$> arbitraryReducedMaybe n -- securityQuestionAnswer :: Maybe Text
    <*> arbitraryReducedMaybe n -- securityQuestionQuestion :: Maybe Text
    <*> arbitraryReducedMaybe n -- securityQuestionQuestionText :: Maybe Text
  
instance Arbitrary SecurityQuestionUserFactor where
  arbitrary = sized genSecurityQuestionUserFactor

genSecurityQuestionUserFactor :: Int -> Gen SecurityQuestionUserFactor
genSecurityQuestionUserFactor n =
  SecurityQuestionUserFactor
    <$> arbitraryReducedMaybe n -- securityQuestionUserFactorProfile :: Maybe SecurityQuestionUserFactorProfile
  
instance Arbitrary SecurityQuestionUserFactorProfile where
  arbitrary = sized genSecurityQuestionUserFactorProfile

genSecurityQuestionUserFactorProfile :: Int -> Gen SecurityQuestionUserFactorProfile
genSecurityQuestionUserFactorProfile n =
  SecurityQuestionUserFactorProfile
    <$> arbitraryReducedMaybe n -- securityQuestionUserFactorProfileAnswer :: Maybe Text
    <*> arbitraryReducedMaybe n -- securityQuestionUserFactorProfileQuestion :: Maybe Text
    <*> arbitraryReducedMaybe n -- securityQuestionUserFactorProfileQuestionText :: Maybe Text
  
instance Arbitrary Session where
  arbitrary = sized genSession

genSession :: Int -> Gen Session
genSession n =
  Session
    <$> arbitraryReducedMaybe n -- sessionLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- sessionAmr :: Maybe [SessionAuthenticationMethod]
    <*> arbitraryReducedMaybe n -- sessionCreatedAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- sessionExpiresAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- sessionId :: Maybe Text
    <*> arbitraryReducedMaybe n -- sessionIdp :: Maybe SessionIdentityProvider
    <*> arbitraryReducedMaybe n -- sessionLastFactorVerification :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- sessionLastPasswordVerification :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- sessionLogin :: Maybe Text
    <*> arbitraryReducedMaybe n -- sessionStatus :: Maybe SessionStatus
    <*> arbitraryReducedMaybe n -- sessionUserId :: Maybe Text
  
instance Arbitrary SessionIdentityProvider where
  arbitrary = sized genSessionIdentityProvider

genSessionIdentityProvider :: Int -> Gen SessionIdentityProvider
genSessionIdentityProvider n =
  SessionIdentityProvider
    <$> arbitraryReducedMaybe n -- sessionIdentityProviderId :: Maybe Text
    <*> arbitraryReducedMaybe n -- sessionIdentityProviderType :: Maybe SessionIdentityProviderType
  
instance Arbitrary SignOnInlineHook where
  arbitrary = sized genSignOnInlineHook

genSignOnInlineHook :: Int -> Gen SignOnInlineHook
genSignOnInlineHook n =
  SignOnInlineHook
    <$> arbitraryReducedMaybe n -- signOnInlineHookId :: Maybe Text
  
instance Arbitrary SingleLogout where
  arbitrary = sized genSingleLogout

genSingleLogout :: Int -> Gen SingleLogout
genSingleLogout n =
  SingleLogout
    <$> arbitraryReducedMaybe n -- singleLogoutEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- singleLogoutIssuer :: Maybe Text
    <*> arbitraryReducedMaybe n -- singleLogoutLogoutUrl :: Maybe Text
  
instance Arbitrary SmsTemplate where
  arbitrary = sized genSmsTemplate

genSmsTemplate :: Int -> Gen SmsTemplate
genSmsTemplate n =
  SmsTemplate
    <$> arbitraryReducedMaybe n -- smsTemplateCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- smsTemplateId :: Maybe Text
    <*> arbitraryReducedMaybe n -- smsTemplateLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- smsTemplateName :: Maybe Text
    <*> arbitraryReducedMaybe n -- smsTemplateTemplate :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- smsTemplateTranslations :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- smsTemplateType :: Maybe SmsTemplateType
  
instance Arbitrary SmsUserFactor where
  arbitrary = sized genSmsUserFactor

genSmsUserFactor :: Int -> Gen SmsUserFactor
genSmsUserFactor n =
  SmsUserFactor
    <$> arbitraryReducedMaybe n -- smsUserFactorProfile :: Maybe SmsUserFactorProfile
  
instance Arbitrary SmsUserFactorProfile where
  arbitrary = sized genSmsUserFactorProfile

genSmsUserFactorProfile :: Int -> Gen SmsUserFactorProfile
genSmsUserFactorProfile n =
  SmsUserFactorProfile
    <$> arbitraryReducedMaybe n -- smsUserFactorProfilePhoneNumber :: Maybe Text
  
instance Arbitrary SocialAuthToken where
  arbitrary = sized genSocialAuthToken

genSocialAuthToken :: Int -> Gen SocialAuthToken
genSocialAuthToken n =
  SocialAuthToken
    <$> arbitraryReducedMaybe n -- socialAuthTokenExpiresAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- socialAuthTokenId :: Maybe Text
    <*> arbitraryReducedMaybe n -- socialAuthTokenScopes :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- socialAuthTokenToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- socialAuthTokenTokenAuthScheme :: Maybe Text
    <*> arbitraryReducedMaybe n -- socialAuthTokenTokenType :: Maybe Text
  
instance Arbitrary SpCertificate where
  arbitrary = sized genSpCertificate

genSpCertificate :: Int -> Gen SpCertificate
genSpCertificate n =
  SpCertificate
    <$> arbitraryReducedMaybe n -- spCertificateX5c :: Maybe [Text]
  
instance Arbitrary Subscription where
  arbitrary = sized genSubscription

genSubscription :: Int -> Gen Subscription
genSubscription n =
  Subscription
    <$> arbitraryReducedMaybe n -- subscriptionNotificationType :: Maybe NotificationType
    <*> arbitraryReducedMaybe n -- subscriptionChannels :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- subscriptionStatus :: Maybe SubscriptionStatus
    <*> arbitraryReducedMaybe n -- subscriptionLinks :: Maybe (Map.Map String A.Value)
  
instance Arbitrary SwaApplication where
  arbitrary = sized genSwaApplication

genSwaApplication :: Int -> Gen SwaApplication
genSwaApplication n =
  SwaApplication
    <$> arbitraryReducedMaybeValue n -- swaApplicationName :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- swaApplicationSettings :: Maybe SwaApplicationSettings
  
instance Arbitrary SwaApplicationSettings where
  arbitrary = sized genSwaApplicationSettings

genSwaApplicationSettings :: Int -> Gen SwaApplicationSettings
genSwaApplicationSettings n =
  SwaApplicationSettings
    <$> arbitraryReducedMaybe n -- swaApplicationSettingsApp :: Maybe SwaApplicationSettingsApplication
  
instance Arbitrary SwaApplicationSettingsApplication where
  arbitrary = sized genSwaApplicationSettingsApplication

genSwaApplicationSettingsApplication :: Int -> Gen SwaApplicationSettingsApplication
genSwaApplicationSettingsApplication n =
  SwaApplicationSettingsApplication
    <$> arbitraryReducedMaybe n -- swaApplicationSettingsApplicationButtonField :: Maybe Text
    <*> arbitraryReducedMaybe n -- swaApplicationSettingsApplicationLoginUrlRegex :: Maybe Text
    <*> arbitraryReducedMaybe n -- swaApplicationSettingsApplicationPasswordField :: Maybe Text
    <*> arbitraryReducedMaybe n -- swaApplicationSettingsApplicationUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- swaApplicationSettingsApplicationUsernameField :: Maybe Text
    <*> arbitraryReducedMaybe n -- swaApplicationSettingsApplicationRedirectUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- swaApplicationSettingsApplicationCheckbox :: Maybe Text
  
instance Arbitrary SwaThreeFieldApplication where
  arbitrary = sized genSwaThreeFieldApplication

genSwaThreeFieldApplication :: Int -> Gen SwaThreeFieldApplication
genSwaThreeFieldApplication n =
  SwaThreeFieldApplication
    <$> arbitraryReducedMaybeValue n -- swaThreeFieldApplicationName :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- swaThreeFieldApplicationSettings :: Maybe SwaThreeFieldApplicationSettings
  
instance Arbitrary SwaThreeFieldApplicationSettings where
  arbitrary = sized genSwaThreeFieldApplicationSettings

genSwaThreeFieldApplicationSettings :: Int -> Gen SwaThreeFieldApplicationSettings
genSwaThreeFieldApplicationSettings n =
  SwaThreeFieldApplicationSettings
    <$> arbitraryReducedMaybe n -- swaThreeFieldApplicationSettingsApp :: Maybe SwaThreeFieldApplicationSettingsApplication
  
instance Arbitrary SwaThreeFieldApplicationSettingsApplication where
  arbitrary = sized genSwaThreeFieldApplicationSettingsApplication

genSwaThreeFieldApplicationSettingsApplication :: Int -> Gen SwaThreeFieldApplicationSettingsApplication
genSwaThreeFieldApplicationSettingsApplication n =
  SwaThreeFieldApplicationSettingsApplication
    <$> arbitraryReducedMaybe n -- swaThreeFieldApplicationSettingsApplicationButtonSelector :: Maybe Text
    <*> arbitraryReducedMaybe n -- swaThreeFieldApplicationSettingsApplicationExtraFieldSelector :: Maybe Text
    <*> arbitraryReducedMaybe n -- swaThreeFieldApplicationSettingsApplicationExtraFieldValue :: Maybe Text
    <*> arbitraryReducedMaybe n -- swaThreeFieldApplicationSettingsApplicationLoginUrlRegex :: Maybe Text
    <*> arbitraryReducedMaybe n -- swaThreeFieldApplicationSettingsApplicationPasswordSelector :: Maybe Text
    <*> arbitraryReducedMaybe n -- swaThreeFieldApplicationSettingsApplicationTargetUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- swaThreeFieldApplicationSettingsApplicationUserNameSelector :: Maybe Text
  
instance Arbitrary TempPassword where
  arbitrary = sized genTempPassword

genTempPassword :: Int -> Gen TempPassword
genTempPassword n =
  TempPassword
    <$> arbitraryReducedMaybe n -- tempPasswordTempPassword :: Maybe Text
  
instance Arbitrary Theme where
  arbitrary = sized genTheme

genTheme :: Int -> Gen Theme
genTheme n =
  Theme
    <$> arbitraryReducedMaybe n -- themeBackgroundImage :: Maybe Text
    <*> arbitraryReducedMaybe n -- themePrimaryColorHex :: Maybe Text
    <*> arbitraryReducedMaybe n -- themePrimaryColorContrastHex :: Maybe Text
    <*> arbitraryReducedMaybe n -- themeSecondaryColorHex :: Maybe Text
    <*> arbitraryReducedMaybe n -- themeSecondaryColorContrastHex :: Maybe Text
    <*> arbitraryReducedMaybe n -- themeSignInPageTouchPointVariant :: Maybe SignInPageTouchPointVariant
    <*> arbitraryReducedMaybe n -- themeEndUserDashboardTouchPointVariant :: Maybe EndUserDashboardTouchPointVariant
    <*> arbitraryReducedMaybe n -- themeErrorPageTouchPointVariant :: Maybe ErrorPageTouchPointVariant
    <*> arbitraryReducedMaybe n -- themeEmailTemplateTouchPointVariant :: Maybe EmailTemplateTouchPointVariant
    <*> arbitraryReducedMaybe n -- themeLinks :: Maybe (Map.Map String A.Value)
  
instance Arbitrary ThemeResponse where
  arbitrary = sized genThemeResponse

genThemeResponse :: Int -> Gen ThemeResponse
genThemeResponse n =
  ThemeResponse
    <$> arbitraryReducedMaybe n -- themeResponseId :: Maybe Text
    <*> arbitraryReducedMaybe n -- themeResponseLogo :: Maybe Text
    <*> arbitraryReducedMaybe n -- themeResponseFavicon :: Maybe Text
    <*> arbitraryReducedMaybe n -- themeResponseBackgroundImage :: Maybe Text
    <*> arbitraryReducedMaybe n -- themeResponsePrimaryColorHex :: Maybe Text
    <*> arbitraryReducedMaybe n -- themeResponsePrimaryColorContrastHex :: Maybe Text
    <*> arbitraryReducedMaybe n -- themeResponseSecondaryColorHex :: Maybe Text
    <*> arbitraryReducedMaybe n -- themeResponseSecondaryColorContrastHex :: Maybe Text
    <*> arbitraryReducedMaybe n -- themeResponseSignInPageTouchPointVariant :: Maybe SignInPageTouchPointVariant
    <*> arbitraryReducedMaybe n -- themeResponseEndUserDashboardTouchPointVariant :: Maybe EndUserDashboardTouchPointVariant
    <*> arbitraryReducedMaybe n -- themeResponseErrorPageTouchPointVariant :: Maybe ErrorPageTouchPointVariant
    <*> arbitraryReducedMaybe n -- themeResponseEmailTemplateTouchPointVariant :: Maybe EmailTemplateTouchPointVariant
    <*> arbitraryReducedMaybe n -- themeResponseLinks :: Maybe (Map.Map String A.Value)
  
instance Arbitrary ThreatInsightConfiguration where
  arbitrary = sized genThreatInsightConfiguration

genThreatInsightConfiguration :: Int -> Gen ThreatInsightConfiguration
genThreatInsightConfiguration n =
  ThreatInsightConfiguration
    <$> arbitraryReducedMaybe n -- threatInsightConfigurationAction :: Maybe Text
    <*> arbitraryReducedMaybe n -- threatInsightConfigurationExcludeZones :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- threatInsightConfigurationCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- threatInsightConfigurationLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- threatInsightConfigurationLinks :: Maybe (Map.Map String A.Value)
  
instance Arbitrary TokenAuthorizationServerPolicyRuleAction where
  arbitrary = sized genTokenAuthorizationServerPolicyRuleAction

genTokenAuthorizationServerPolicyRuleAction :: Int -> Gen TokenAuthorizationServerPolicyRuleAction
genTokenAuthorizationServerPolicyRuleAction n =
  TokenAuthorizationServerPolicyRuleAction
    <$> arbitraryReducedMaybe n -- tokenAuthorizationServerPolicyRuleActionAccessTokenLifetimeMinutes :: Maybe Int
    <*> arbitraryReducedMaybe n -- tokenAuthorizationServerPolicyRuleActionRefreshTokenLifetimeMinutes :: Maybe Int
    <*> arbitraryReducedMaybe n -- tokenAuthorizationServerPolicyRuleActionRefreshTokenWindowMinutes :: Maybe Int
    <*> arbitraryReducedMaybe n -- tokenAuthorizationServerPolicyRuleActionInlineHook :: Maybe TokenAuthorizationServerPolicyRuleActionInlineHook
  
instance Arbitrary TokenAuthorizationServerPolicyRuleActionInlineHook where
  arbitrary = sized genTokenAuthorizationServerPolicyRuleActionInlineHook

genTokenAuthorizationServerPolicyRuleActionInlineHook :: Int -> Gen TokenAuthorizationServerPolicyRuleActionInlineHook
genTokenAuthorizationServerPolicyRuleActionInlineHook n =
  TokenAuthorizationServerPolicyRuleActionInlineHook
    <$> arbitraryReducedMaybe n -- tokenAuthorizationServerPolicyRuleActionInlineHookId :: Maybe Text
  
instance Arbitrary TokenUserFactor where
  arbitrary = sized genTokenUserFactor

genTokenUserFactor :: Int -> Gen TokenUserFactor
genTokenUserFactor n =
  TokenUserFactor
    <$> arbitraryReducedMaybe n -- tokenUserFactorProfile :: Maybe TokenUserFactorProfile
  
instance Arbitrary TokenUserFactorProfile where
  arbitrary = sized genTokenUserFactorProfile

genTokenUserFactorProfile :: Int -> Gen TokenUserFactorProfile
genTokenUserFactorProfile n =
  TokenUserFactorProfile
    <$> arbitraryReducedMaybe n -- tokenUserFactorProfileCredentialId :: Maybe Text
  
instance Arbitrary TotpUserFactor where
  arbitrary = sized genTotpUserFactor

genTotpUserFactor :: Int -> Gen TotpUserFactor
genTotpUserFactor n =
  TotpUserFactor
    <$> arbitraryReducedMaybe n -- totpUserFactorProfile :: Maybe TotpUserFactorProfile
  
instance Arbitrary TotpUserFactorProfile where
  arbitrary = sized genTotpUserFactorProfile

genTotpUserFactorProfile :: Int -> Gen TotpUserFactorProfile
genTotpUserFactorProfile n =
  TotpUserFactorProfile
    <$> arbitraryReducedMaybe n -- totpUserFactorProfileCredentialId :: Maybe Text
  
instance Arbitrary TrustedOrigin where
  arbitrary = sized genTrustedOrigin

genTrustedOrigin :: Int -> Gen TrustedOrigin
genTrustedOrigin n =
  TrustedOrigin
    <$> arbitraryReducedMaybe n -- trustedOriginLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- trustedOriginCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- trustedOriginCreatedBy :: Maybe Text
    <*> arbitraryReducedMaybe n -- trustedOriginId :: Maybe Text
    <*> arbitraryReducedMaybe n -- trustedOriginLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- trustedOriginLastUpdatedBy :: Maybe Text
    <*> arbitraryReducedMaybe n -- trustedOriginName :: Maybe Text
    <*> arbitraryReducedMaybe n -- trustedOriginOrigin :: Maybe Text
    <*> arbitraryReducedMaybe n -- trustedOriginScopes :: Maybe [Scope]
    <*> arbitraryReducedMaybe n -- trustedOriginStatus :: Maybe Text
  
instance Arbitrary U2fUserFactor where
  arbitrary = sized genU2fUserFactor

genU2fUserFactor :: Int -> Gen U2fUserFactor
genU2fUserFactor n =
  U2fUserFactor
    <$> arbitraryReducedMaybe n -- u2fUserFactorProfile :: Maybe U2fUserFactorProfile
  
instance Arbitrary U2fUserFactorProfile where
  arbitrary = sized genU2fUserFactorProfile

genU2fUserFactorProfile :: Int -> Gen U2fUserFactorProfile
genU2fUserFactorProfile n =
  U2fUserFactorProfile
    <$> arbitraryReducedMaybe n -- u2fUserFactorProfileCredentialId :: Maybe Text
  
instance Arbitrary User' where
  arbitrary = sized genUser

genUser :: Int -> Gen User'
genUser n =
  User
    <$> arbitraryReducedMaybe n -- userEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- userLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- userActivated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userCredentials :: Maybe UserCredentials
    <*> arbitraryReducedMaybe n -- userGroupIds :: !(Maybe [Text]) -- ^ "groupIds"
    <*> arbitraryReducedMaybe n -- userId :: Maybe Text
    <*> arbitraryReducedMaybe n -- userLastLogin :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userPasswordChanged :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userProfile :: Maybe UserProfile
    <*> arbitraryReducedMaybe n -- userStatus :: Maybe UserStatus
    <*> arbitraryReducedMaybe n -- userStatusChanged :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userTransitioningToStatus :: Maybe UserStatus
    <*> arbitraryReducedMaybe n -- userType :: Maybe UserType
  
instance Arbitrary UserActivationToken where
  arbitrary = sized genUserActivationToken

genUserActivationToken :: Int -> Gen UserActivationToken
genUserActivationToken n =
  UserActivationToken
    <$> arbitraryReducedMaybe n -- userActivationTokenActivationToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- userActivationTokenActivationUrl :: Maybe Text
  
instance Arbitrary UserCondition where
  arbitrary = sized genUserCondition

genUserCondition :: Int -> Gen UserCondition
genUserCondition n =
  UserCondition
    <$> arbitraryReducedMaybe n -- userConditionExclude :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- userConditionInclude :: Maybe [Text]
  
instance Arbitrary UserCredentials where
  arbitrary = sized genUserCredentials

genUserCredentials :: Int -> Gen UserCredentials
genUserCredentials n =
  UserCredentials
    <$> arbitraryReducedMaybe n -- userCredentialsPassword :: Maybe PasswordCredential
    <*> arbitraryReducedMaybe n -- userCredentialsProvider :: Maybe AuthenticationProvider
    <*> arbitraryReducedMaybe n -- userCredentialsRecoveryQuestion :: Maybe RecoveryQuestionCredential
  
instance Arbitrary UserFactor where
  arbitrary = sized genUserFactor

genUserFactor :: Int -> Gen UserFactor
genUserFactor n =
  UserFactor
    <$> arbitraryReducedMaybe n -- userFactorEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- userFactorLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- userFactorCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userFactorFactorType :: Maybe FactorType
    <*> arbitraryReducedMaybe n -- userFactorId :: Maybe Text
    <*> arbitraryReducedMaybe n -- userFactorLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userFactorProvider :: Maybe FactorProvider
    <*> arbitraryReducedMaybe n -- userFactorStatus :: Maybe FactorStatus
    <*> arbitraryReducedMaybe n -- userFactorVerify :: Maybe VerifyFactorRequest
  
instance Arbitrary UserIdString where
  arbitrary = sized genUserIdString

genUserIdString :: Int -> Gen UserIdString
genUserIdString n =
  UserIdString
    <$> arbitraryReducedMaybe n -- userIdStringUserId :: Maybe Text
  
instance Arbitrary UserIdentifierConditionEvaluatorPattern where
  arbitrary = sized genUserIdentifierConditionEvaluatorPattern

genUserIdentifierConditionEvaluatorPattern :: Int -> Gen UserIdentifierConditionEvaluatorPattern
genUserIdentifierConditionEvaluatorPattern n =
  UserIdentifierConditionEvaluatorPattern
    <$> arbitraryReducedMaybe n -- userIdentifierConditionEvaluatorPatternMatchType :: Maybe E'MatchType2
    <*> arbitraryReducedMaybe n -- userIdentifierConditionEvaluatorPatternValue :: Maybe Text
  
instance Arbitrary UserIdentifierPolicyRuleCondition where
  arbitrary = sized genUserIdentifierPolicyRuleCondition

genUserIdentifierPolicyRuleCondition :: Int -> Gen UserIdentifierPolicyRuleCondition
genUserIdentifierPolicyRuleCondition n =
  UserIdentifierPolicyRuleCondition
    <$> arbitraryReducedMaybe n -- userIdentifierPolicyRuleConditionAttribute :: Maybe Text
    <*> arbitraryReducedMaybe n -- userIdentifierPolicyRuleConditionPatterns :: Maybe [UserIdentifierConditionEvaluatorPattern]
    <*> arbitraryReducedMaybe n -- userIdentifierPolicyRuleConditionType :: Maybe E'Type11
  
instance Arbitrary UserIdentityProviderLinkRequest where
  arbitrary = sized genUserIdentityProviderLinkRequest

genUserIdentityProviderLinkRequest :: Int -> Gen UserIdentityProviderLinkRequest
genUserIdentityProviderLinkRequest n =
  UserIdentityProviderLinkRequest
    <$> arbitraryReducedMaybe n -- userIdentityProviderLinkRequestExternalId :: Maybe Text
  
instance Arbitrary UserLifecycleAttributePolicyRuleCondition where
  arbitrary = sized genUserLifecycleAttributePolicyRuleCondition

genUserLifecycleAttributePolicyRuleCondition :: Int -> Gen UserLifecycleAttributePolicyRuleCondition
genUserLifecycleAttributePolicyRuleCondition n =
  UserLifecycleAttributePolicyRuleCondition
    <$> arbitraryReducedMaybe n -- userLifecycleAttributePolicyRuleConditionAttributeName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userLifecycleAttributePolicyRuleConditionMatchingValue :: Maybe Text
  
instance Arbitrary UserPolicyRuleCondition where
  arbitrary = sized genUserPolicyRuleCondition

genUserPolicyRuleCondition :: Int -> Gen UserPolicyRuleCondition
genUserPolicyRuleCondition n =
  UserPolicyRuleCondition
    <$> arbitraryReducedMaybe n -- userPolicyRuleConditionExclude :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- userPolicyRuleConditionInactivity :: Maybe InactivityPolicyRuleCondition
    <*> arbitraryReducedMaybe n -- userPolicyRuleConditionInclude :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- userPolicyRuleConditionLifecycleExpiration :: Maybe LifecycleExpirationPolicyRuleCondition
    <*> arbitraryReducedMaybe n -- userPolicyRuleConditionPasswordExpiration :: Maybe PasswordExpirationPolicyRuleCondition
    <*> arbitraryReducedMaybe n -- userPolicyRuleConditionUserLifecycleAttribute :: Maybe UserLifecycleAttributePolicyRuleCondition
  
instance Arbitrary UserProfile where
  arbitrary = sized genUserProfile

genUserProfile :: Int -> Gen UserProfile
genUserProfile n =
  UserProfile
    <$> arbitraryReducedMaybe n -- userProfileCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileCostCenter :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileCountryCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileDepartment :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileDisplayName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileDivision :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileEmployeeNumber :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileFirstName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileHonorificPrefix :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileHonorificSuffix :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileLastName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileLocale :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileLogin :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileManager :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileManagerId :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileMiddleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileMobilePhone :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileNickName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileOrganization :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfilePostalAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfilePreferredLanguage :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfilePrimaryPhone :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileProfileUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileSecondEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileState :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileStreetAddress :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileTimezone :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileUserType :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileZipCode :: Maybe Text
  
instance Arbitrary UserSchema where
  arbitrary = sized genUserSchema

genUserSchema :: Int -> Gen UserSchema
genUserSchema n =
  UserSchema
    <$> arbitraryReducedMaybe n -- userSchemaId :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaSchema :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaLastUpdated :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaDefinitions :: Maybe UserSchemaDefinitions
    <*> arbitraryReducedMaybe n -- userSchemaType :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaProperties :: Maybe UserSchemaProperties
    <*> arbitraryReducedMaybe n -- userSchemaLinks :: Maybe (Map.Map String A.Value)
  
instance Arbitrary UserSchemaAttribute where
  arbitrary = sized genUserSchemaAttribute

genUserSchemaAttribute :: Int -> Gen UserSchemaAttribute
genUserSchemaAttribute n =
  UserSchemaAttribute
    <$> arbitraryReducedMaybe n -- userSchemaAttributeTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaAttributeType :: Maybe UserSchemaAttributeType
    <*> arbitraryReducedMaybe n -- userSchemaAttributeRequired :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userSchemaAttributeMutability :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaAttributeScope :: Maybe UserSchemaAttributeScope
    <*> arbitraryReducedMaybe n -- userSchemaAttributeEnum :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- userSchemaAttributeOneOf :: Maybe [UserSchemaAttributeEnum]
    <*> arbitraryReducedMaybe n -- userSchemaAttributeMinLength :: Maybe Int
    <*> arbitraryReducedMaybe n -- userSchemaAttributeMaxLength :: Maybe Int
    <*> arbitraryReducedMaybe n -- userSchemaAttributeDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaAttributePermissions :: Maybe [UserSchemaAttributePermission]
    <*> arbitraryReducedMaybe n -- userSchemaAttributeMaster :: Maybe UserSchemaAttributeMaster
    <*> arbitraryReducedMaybe n -- userSchemaAttributeUnion :: Maybe UserSchemaAttributeUnion
    <*> arbitraryReducedMaybe n -- userSchemaAttributeItems :: Maybe UserSchemaAttributeItems
    <*> arbitraryReducedMaybe n -- userSchemaAttributePattern :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaAttributeUnique :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaAttributeExternalName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaAttributeExternalNamespace :: Maybe Text
  
instance Arbitrary UserSchemaAttributeEnum where
  arbitrary = sized genUserSchemaAttributeEnum

genUserSchemaAttributeEnum :: Int -> Gen UserSchemaAttributeEnum
genUserSchemaAttributeEnum n =
  UserSchemaAttributeEnum
    <$> arbitraryReducedMaybe n -- userSchemaAttributeEnumConst :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaAttributeEnumTitle :: Maybe Text
  
instance Arbitrary UserSchemaAttributeItems where
  arbitrary = sized genUserSchemaAttributeItems

genUserSchemaAttributeItems :: Int -> Gen UserSchemaAttributeItems
genUserSchemaAttributeItems n =
  UserSchemaAttributeItems
    <$> arbitraryReducedMaybe n -- userSchemaAttributeItemsEnum :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- userSchemaAttributeItemsOneOf :: Maybe [UserSchemaAttributeEnum]
    <*> arbitraryReducedMaybe n -- userSchemaAttributeItemsType :: Maybe Text
  
instance Arbitrary UserSchemaAttributeMaster where
  arbitrary = sized genUserSchemaAttributeMaster

genUserSchemaAttributeMaster :: Int -> Gen UserSchemaAttributeMaster
genUserSchemaAttributeMaster n =
  UserSchemaAttributeMaster
    <$> arbitraryReducedMaybe n -- userSchemaAttributeMasterType :: Maybe UserSchemaAttributeMasterType
    <*> arbitraryReducedMaybe n -- userSchemaAttributeMasterPriority :: Maybe [UserSchemaAttributeMasterPriority]
  
instance Arbitrary UserSchemaAttributeMasterPriority where
  arbitrary = sized genUserSchemaAttributeMasterPriority

genUserSchemaAttributeMasterPriority :: Int -> Gen UserSchemaAttributeMasterPriority
genUserSchemaAttributeMasterPriority n =
  UserSchemaAttributeMasterPriority
    <$> arbitraryReducedMaybe n -- userSchemaAttributeMasterPriorityType :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaAttributeMasterPriorityValue :: Maybe Text
  
instance Arbitrary UserSchemaAttributePermission where
  arbitrary = sized genUserSchemaAttributePermission

genUserSchemaAttributePermission :: Int -> Gen UserSchemaAttributePermission
genUserSchemaAttributePermission n =
  UserSchemaAttributePermission
    <$> arbitraryReducedMaybe n -- userSchemaAttributePermissionPrincipal :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaAttributePermissionAction :: Maybe Text
  
instance Arbitrary UserSchemaAttributeScope where
  arbitrary = sized genUserSchemaAttributeScope

genUserSchemaAttributeScope :: Int -> Gen UserSchemaAttributeScope
genUserSchemaAttributeScope n =
  
  pure UserSchemaAttributeScope
   
instance Arbitrary UserSchemaAttributeUnion where
  arbitrary = sized genUserSchemaAttributeUnion

genUserSchemaAttributeUnion :: Int -> Gen UserSchemaAttributeUnion
genUserSchemaAttributeUnion n =
  
  pure UserSchemaAttributeUnion
   
instance Arbitrary UserSchemaBase where
  arbitrary = sized genUserSchemaBase

genUserSchemaBase :: Int -> Gen UserSchemaBase
genUserSchemaBase n =
  UserSchemaBase
    <$> arbitraryReducedMaybe n -- userSchemaBaseId :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaBaseType :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaBaseProperties :: Maybe UserSchemaBaseProperties
    <*> arbitraryReducedMaybe n -- userSchemaBaseRequired :: Maybe [Text]
  
instance Arbitrary UserSchemaBaseProperties where
  arbitrary = sized genUserSchemaBaseProperties

genUserSchemaBaseProperties :: Int -> Gen UserSchemaBaseProperties
genUserSchemaBaseProperties n =
  UserSchemaBaseProperties
    <$> arbitraryReducedMaybe n -- userSchemaBasePropertiesLogin :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesFirstName :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesLastName :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesMiddleName :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesHonorificPrefix :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesHonorificSuffix :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesEmail :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesTitle :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesDisplayName :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesNickName :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesProfileUrl :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesSecondEmail :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesMobilePhone :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesPrimaryPhone :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesStreetAddress :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesCity :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesState :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesZipCode :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesCountryCode :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesPostalAddress :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesPreferredLanguage :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesLocale :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesTimezone :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesUserType :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesEmployeeNumber :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesCostCenter :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesOrganization :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesDivision :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesDepartment :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesManagerId :: Maybe UserSchemaAttribute
    <*> arbitraryReducedMaybe n -- userSchemaBasePropertiesManager :: Maybe UserSchemaAttribute
  
instance Arbitrary UserSchemaDefinitions where
  arbitrary = sized genUserSchemaDefinitions

genUserSchemaDefinitions :: Int -> Gen UserSchemaDefinitions
genUserSchemaDefinitions n =
  UserSchemaDefinitions
    <$> arbitraryReducedMaybe n -- userSchemaDefinitionsBase :: Maybe UserSchemaBase
    <*> arbitraryReducedMaybe n -- userSchemaDefinitionsCustom :: Maybe UserSchemaPublic
  
instance Arbitrary UserSchemaProperties where
  arbitrary = sized genUserSchemaProperties

genUserSchemaProperties :: Int -> Gen UserSchemaProperties
genUserSchemaProperties n =
  UserSchemaProperties
    <$> arbitraryReducedMaybe n -- userSchemaPropertiesProfile :: Maybe UserSchemaPropertiesProfile
  
instance Arbitrary UserSchemaPropertiesProfile where
  arbitrary = sized genUserSchemaPropertiesProfile

genUserSchemaPropertiesProfile :: Int -> Gen UserSchemaPropertiesProfile
genUserSchemaPropertiesProfile n =
  UserSchemaPropertiesProfile
    <$> arbitraryReducedMaybe n -- userSchemaPropertiesProfileAllOf :: Maybe [UserSchemaPropertiesProfileItem]
  
instance Arbitrary UserSchemaPropertiesProfileItem where
  arbitrary = sized genUserSchemaPropertiesProfileItem

genUserSchemaPropertiesProfileItem :: Int -> Gen UserSchemaPropertiesProfileItem
genUserSchemaPropertiesProfileItem n =
  UserSchemaPropertiesProfileItem
    <$> arbitraryReducedMaybe n -- userSchemaPropertiesProfileItemRef :: Maybe Text
  
instance Arbitrary UserSchemaPublic where
  arbitrary = sized genUserSchemaPublic

genUserSchemaPublic :: Int -> Gen UserSchemaPublic
genUserSchemaPublic n =
  UserSchemaPublic
    <$> arbitraryReducedMaybe n -- userSchemaPublicId :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaPublicType :: Maybe Text
    <*> arbitraryReducedMaybe n -- userSchemaPublicProperties :: Maybe (Map.Map String UserSchemaAttribute)
    <*> arbitraryReducedMaybe n -- userSchemaPublicRequired :: Maybe [Text]
  
instance Arbitrary UserStatusPolicyRuleCondition where
  arbitrary = sized genUserStatusPolicyRuleCondition

genUserStatusPolicyRuleCondition :: Int -> Gen UserStatusPolicyRuleCondition
genUserStatusPolicyRuleCondition n =
  UserStatusPolicyRuleCondition
    <$> arbitraryReducedMaybe n -- userStatusPolicyRuleConditionValue :: Maybe E'Status4
  
instance Arbitrary UserType where
  arbitrary = sized genUserType

genUserType :: Int -> Gen UserType
genUserType n =
  UserType
    <$> arbitraryReducedMaybe n -- userTypeLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- userTypeCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userTypeCreatedBy :: Maybe Text
    <*> arbitraryReducedMaybe n -- userTypeDefault :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userTypeDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- userTypeDisplayName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userTypeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- userTypeLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userTypeLastUpdatedBy :: Maybe Text
    <*> arbitraryReducedMaybe n -- userTypeName :: Maybe Text
  
instance Arbitrary UserTypeCondition where
  arbitrary = sized genUserTypeCondition

genUserTypeCondition :: Int -> Gen UserTypeCondition
genUserTypeCondition n =
  UserTypeCondition
    <$> arbitraryReducedMaybe n -- userTypeConditionExclude :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- userTypeConditionInclude :: Maybe [Text]
  
instance Arbitrary VerificationMethod where
  arbitrary = sized genVerificationMethod

genVerificationMethod :: Int -> Gen VerificationMethod
genVerificationMethod n =
  VerificationMethod
    <$> arbitraryReducedMaybe n -- verificationMethodFactorMode :: Maybe Text
    <*> arbitraryReducedMaybe n -- verificationMethodType :: Maybe Text
    <*> arbitraryReducedMaybe n -- verificationMethodReauthenticateIn :: Maybe Text
    <*> arbitraryReducedMaybe n -- verificationMethodConstraints :: Maybe [AccessPolicyConstraints]
    <*> arbitraryReducedMaybe n -- verificationMethodInactivityPeriod :: Maybe Text
  
instance Arbitrary VerifyFactorRequest where
  arbitrary = sized genVerifyFactorRequest

genVerifyFactorRequest :: Int -> Gen VerifyFactorRequest
genVerifyFactorRequest n =
  VerifyFactorRequest
    <$> arbitraryReducedMaybe n -- verifyFactorRequestActivationToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- verifyFactorRequestAnswer :: Maybe Text
    <*> arbitraryReducedMaybe n -- verifyFactorRequestAttestation :: Maybe Text
    <*> arbitraryReducedMaybe n -- verifyFactorRequestClientData :: Maybe Text
    <*> arbitraryReducedMaybe n -- verifyFactorRequestNextPassCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- verifyFactorRequestPassCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- verifyFactorRequestRegistrationData :: Maybe Text
    <*> arbitraryReducedMaybe n -- verifyFactorRequestStateToken :: Maybe Text
  
instance Arbitrary VerifyUserFactorResponse where
  arbitrary = sized genVerifyUserFactorResponse

genVerifyUserFactorResponse :: Int -> Gen VerifyUserFactorResponse
genVerifyUserFactorResponse n =
  VerifyUserFactorResponse
    <$> arbitraryReducedMaybe n -- verifyUserFactorResponseEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- verifyUserFactorResponseLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- verifyUserFactorResponseExpiresAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- verifyUserFactorResponseFactorResult :: Maybe E'FactorResult
    <*> arbitraryReducedMaybe n -- verifyUserFactorResponseFactorResultMessage :: Maybe Text
  
instance Arbitrary WebAuthnUserFactor where
  arbitrary = sized genWebAuthnUserFactor

genWebAuthnUserFactor :: Int -> Gen WebAuthnUserFactor
genWebAuthnUserFactor n =
  WebAuthnUserFactor
    <$> arbitraryReducedMaybe n -- webAuthnUserFactorProfile :: Maybe WebAuthnUserFactorProfile
  
instance Arbitrary WebAuthnUserFactorProfile where
  arbitrary = sized genWebAuthnUserFactorProfile

genWebAuthnUserFactorProfile :: Int -> Gen WebAuthnUserFactorProfile
genWebAuthnUserFactorProfile n =
  WebAuthnUserFactorProfile
    <$> arbitraryReducedMaybe n -- webAuthnUserFactorProfileCredentialId :: Maybe Text
    <*> arbitraryReducedMaybe n -- webAuthnUserFactorProfileAuthenticatorName :: Maybe Text
  
instance Arbitrary WebUserFactor where
  arbitrary = sized genWebUserFactor

genWebUserFactor :: Int -> Gen WebUserFactor
genWebUserFactor n =
  WebUserFactor
    <$> arbitraryReducedMaybe n -- webUserFactorProfile :: Maybe WebUserFactorProfile
  
instance Arbitrary WebUserFactorProfile where
  arbitrary = sized genWebUserFactorProfile

genWebUserFactorProfile :: Int -> Gen WebUserFactorProfile
genWebUserFactorProfile n =
  WebUserFactorProfile
    <$> arbitraryReducedMaybe n -- webUserFactorProfileCredentialId :: Maybe Text
  
instance Arbitrary WsFederationApplication where
  arbitrary = sized genWsFederationApplication

genWsFederationApplication :: Int -> Gen WsFederationApplication
genWsFederationApplication n =
  WsFederationApplication
    <$> arbitraryReducedMaybeValue n -- wsFederationApplicationName :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- wsFederationApplicationSettings :: Maybe WsFederationApplicationSettings
  
instance Arbitrary WsFederationApplicationSettings where
  arbitrary = sized genWsFederationApplicationSettings

genWsFederationApplicationSettings :: Int -> Gen WsFederationApplicationSettings
genWsFederationApplicationSettings n =
  WsFederationApplicationSettings
    <$> arbitraryReducedMaybe n -- wsFederationApplicationSettingsApp :: Maybe WsFederationApplicationSettingsApplication
  
instance Arbitrary WsFederationApplicationSettingsApplication where
  arbitrary = sized genWsFederationApplicationSettingsApplication

genWsFederationApplicationSettingsApplication :: Int -> Gen WsFederationApplicationSettingsApplication
genWsFederationApplicationSettingsApplication n =
  WsFederationApplicationSettingsApplication
    <$> arbitraryReducedMaybe n -- wsFederationApplicationSettingsApplicationAttributeStatements :: Maybe Text
    <*> arbitraryReducedMaybe n -- wsFederationApplicationSettingsApplicationAudienceRestriction :: Maybe Text
    <*> arbitraryReducedMaybe n -- wsFederationApplicationSettingsApplicationAuthnContextClassRef :: Maybe Text
    <*> arbitraryReducedMaybe n -- wsFederationApplicationSettingsApplicationGroupFilter :: Maybe Text
    <*> arbitraryReducedMaybe n -- wsFederationApplicationSettingsApplicationGroupName :: Maybe Text
    <*> arbitraryReducedMaybe n -- wsFederationApplicationSettingsApplicationGroupValueFormat :: Maybe Text
    <*> arbitraryReducedMaybe n -- wsFederationApplicationSettingsApplicationNameIdFormat :: Maybe Text
    <*> arbitraryReducedMaybe n -- wsFederationApplicationSettingsApplicationRealm :: Maybe Text
    <*> arbitraryReducedMaybe n -- wsFederationApplicationSettingsApplicationSiteUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- wsFederationApplicationSettingsApplicationUsernameAttribute :: Maybe Text
    <*> arbitraryReducedMaybe n -- wsFederationApplicationSettingsApplicationWReplyOverride :: Maybe Bool
    <*> arbitraryReducedMaybe n -- wsFederationApplicationSettingsApplicationWReplyUrl :: Maybe Text
  



instance Arbitrary AllowedForEnum where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ApplicationCredentialsScheme where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ApplicationCredentialsSigningUse where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ApplicationSignOnMode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary AuthenticationProviderType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary AuthenticatorStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary AuthenticatorType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary AuthorizationServerCredentialsRotationMode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary AuthorizationServerCredentialsUse where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary CatalogApplicationStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ChangeEnum where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary DNSRecordType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary DomainCertificateSourceType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary DomainCertificateType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary DomainValidationStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Access where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Action where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Action2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Action3 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Action4 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Action5 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'AuthType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Binding where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ClaimType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Connection where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Consent where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Enrollment where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'FactorPromptMode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'FactorResult where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'GroupFilterType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'IssuerMode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'MatchType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'MatchType2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'MetadataPublish where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ProfileMappingPropertyPushStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Provider where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Provider2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Revocation where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Scope where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status3 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status4 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'SupportedMdmFrameworks where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'TrustLevel where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type10 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type11 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type3 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type4 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type5 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type6 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type7 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type8 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type9 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Types where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Use where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'UserSchemaAttributeScope where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'UserSchemaAttributeUnion where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'ValueType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'VerificationStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary EmailTemplateTouchPointVariant where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary EnabledStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary EndUserDashboardTouchPointVariant where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ErrorPageTouchPointVariant where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary EventHookChannelConfigAuthSchemeType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary FactorProvider where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary FactorResultType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary FactorStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary FactorType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary FeatureStageState where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary FeatureStageValue where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary FeatureType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary FipsEnum where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary GroupRuleStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary GroupType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary IframeEmbedScopeAllowedApps where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary InlineHookStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary InlineHookType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary LinkedObjectDetailsType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary LogAuthenticationProvider where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary LogCredentialProvider where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary LogCredentialType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary LogSeverity where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary MultifactorEnrollmentPolicyAuthenticatorStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary MultifactorEnrollmentPolicyAuthenticatorType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary MultifactorEnrollmentPolicySettingsType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary NetworkZoneAddressType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary NetworkZoneStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary NetworkZoneType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary NetworkZoneUsage where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary NotificationType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OAuth2ScopeConsentGrantSource where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OAuth2ScopeConsentGrantStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OAuthEndpointAuthenticationMethod where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OAuthGrantType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OAuthResponseType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OpenIdConnectApplicationConsentMethod where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OpenIdConnectApplicationIssuerMode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OpenIdConnectApplicationType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OpenIdConnectRefreshTokenRotationType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OrgContactType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OrgOktaSupportSetting where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PasswordCredentialHashAlgorithm where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PolicyRuleActionsEnrollSelf where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PolicySubjectMatchType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary PolicyType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ProtocolRelayStateFormat where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ProvisioningConnectionAuthScheme where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ProvisioningConnectionStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary RequiredEnum where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary RoleAssignmentType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary RoleStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary RoleType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ScopeType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SeedEnum where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SessionAuthenticationMethod where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SessionIdentityProviderType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SessionStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SignInPageTouchPointVariant where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SmsTemplateType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SubscriptionStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary UserNextLogin where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary UserSchemaAttributeMasterType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary UserSchemaAttributeType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary UserStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary UserVerificationEnum where
  arbitrary = arbitraryBoundedEnum

