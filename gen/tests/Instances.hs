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

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
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
  
instance Arbitrary ApplicationCredentialsUsernameTemplate where
  arbitrary = sized genApplicationCredentialsUsernameTemplate

genApplicationCredentialsUsernameTemplate :: Int -> Gen ApplicationCredentialsUsernameTemplate
genApplicationCredentialsUsernameTemplate n =
  ApplicationCredentialsUsernameTemplate
    <$> arbitraryReducedMaybe n -- applicationCredentialsUsernameTemplateSuffix :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationCredentialsUsernameTemplateTemplate :: Maybe Text
    <*> arbitraryReducedMaybe n -- applicationCredentialsUsernameTemplateType :: Maybe Text
  
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
    <$> arbitraryReducedMaybeValue n -- applicationSettingsApp :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- applicationSettingsImplicitAssignment :: Maybe Bool
    <*> arbitraryReducedMaybe n -- applicationSettingsNotifications :: Maybe ApplicationSettingsNotifications
  
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
    <$> arbitraryReducedMaybe n -- applicationVisibilityAppLinks :: Maybe (Map.Map String Bool)
    <*> arbitraryReducedMaybe n -- applicationVisibilityAutoSubmitToolbar :: Maybe Bool
    <*> arbitraryReducedMaybe n -- applicationVisibilityHide :: Maybe ApplicationVisibilityHide
  
instance Arbitrary ApplicationVisibilityHide where
  arbitrary = sized genApplicationVisibilityHide

genApplicationVisibilityHide :: Int -> Gen ApplicationVisibilityHide
genApplicationVisibilityHide n =
  ApplicationVisibilityHide
    <$> arbitraryReducedMaybe n -- applicationVisibilityHideIOs :: Maybe Bool
    <*> arbitraryReducedMaybe n -- applicationVisibilityHideWeb :: Maybe Bool
  
instance Arbitrary AuthenticationProvider where
  arbitrary = sized genAuthenticationProvider

genAuthenticationProvider :: Int -> Gen AuthenticationProvider
genAuthenticationProvider n =
  AuthenticationProvider
    <$> arbitraryReducedMaybe n -- authenticationProviderName :: Maybe Text
    <*> arbitraryReducedMaybe n -- authenticationProviderType :: Maybe AuthenticationProviderType
  
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
  
instance Arbitrary BrowserPluginApplication where
  arbitrary = sized genBrowserPluginApplication

genBrowserPluginApplication :: Int -> Gen BrowserPluginApplication
genBrowserPluginApplication n =
  BrowserPluginApplication
    <$> arbitraryReducedMaybe n -- browserPluginApplicationCredentials :: Maybe SchemeApplicationCredentials
  
instance Arbitrary CallFactor where
  arbitrary = sized genCallFactor

genCallFactor :: Int -> Gen CallFactor
genCallFactor n =
  CallFactor
    <$> arbitraryReducedMaybe n -- callFactorProfile :: Maybe CallFactorProfile
  
instance Arbitrary CallFactorProfile where
  arbitrary = sized genCallFactorProfile

genCallFactorProfile :: Int -> Gen CallFactorProfile
genCallFactorProfile n =
  CallFactorProfile
    <$> arbitraryReducedMaybe n -- callFactorProfilePhoneExtension :: Maybe Text
    <*> arbitraryReducedMaybe n -- callFactorProfilePhoneNumber :: Maybe Text
  
instance Arbitrary ChangePasswordRequest where
  arbitrary = sized genChangePasswordRequest

genChangePasswordRequest :: Int -> Gen ChangePasswordRequest
genChangePasswordRequest n =
  ChangePasswordRequest
    <$> arbitraryReducedMaybe n -- changePasswordRequestNewPassword :: Maybe PasswordCredential
    <*> arbitraryReducedMaybe n -- changePasswordRequestOldPassword :: Maybe PasswordCredential
  
instance Arbitrary CreateSessionRequest where
  arbitrary = sized genCreateSessionRequest

genCreateSessionRequest :: Int -> Gen CreateSessionRequest
genCreateSessionRequest n =
  CreateSessionRequest
    <$> arbitraryReducedMaybe n -- createSessionRequestSessionToken :: Maybe Text
  
instance Arbitrary EmailAddress where
  arbitrary = sized genEmailAddress

genEmailAddress :: Int -> Gen EmailAddress
genEmailAddress n =
  EmailAddress
    <$> arbitraryReducedMaybe n -- emailAddressStatus :: Maybe EmailStatus
    <*> arbitraryReducedMaybe n -- emailAddressType :: Maybe EmailType
    <*> arbitraryReducedMaybe n -- emailAddressValue :: Maybe Text
  
instance Arbitrary EmailFactor where
  arbitrary = sized genEmailFactor

genEmailFactor :: Int -> Gen EmailFactor
genEmailFactor n =
  EmailFactor
    <$> arbitraryReducedMaybe n -- emailFactorProfile :: Maybe EmailFactorProfile
  
instance Arbitrary EmailFactorProfile where
  arbitrary = sized genEmailFactorProfile

genEmailFactorProfile :: Int -> Gen EmailFactorProfile
genEmailFactorProfile n =
  EmailFactorProfile
    <$> arbitraryReducedMaybe n -- emailFactorProfileEmail :: Maybe Text
  
instance Arbitrary Factor where
  arbitrary = sized genFactor

genFactor :: Int -> Gen Factor
genFactor n =
  Factor
    <$> arbitraryReducedMaybe n -- factorEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- factorLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- factorDevice :: Maybe Text
    <*> arbitraryReducedMaybe n -- factorDeviceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- factorFactorType :: Maybe FactorType
    <*> arbitraryReducedMaybe n -- factorId :: Maybe Text
    <*> arbitraryReducedMaybe n -- factorMfaStateTokenId :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- factorProfile :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- factorProvider :: Maybe FactorProvider
    <*> arbitraryReducedMaybe n -- factorRechallengeExistingFactor :: Maybe Bool
    <*> arbitraryReducedMaybe n -- factorSessionId :: Maybe Text
    <*> arbitraryReducedMaybe n -- factorStatus :: Maybe FactorStatus
    <*> arbitraryReducedMaybe n -- factorTokenLifetimeSeconds :: Maybe Int
    <*> arbitraryReducedMaybe n -- factorUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- factorVerify :: Maybe VerifyFactorRequest
  
instance Arbitrary ForgotPasswordResponse where
  arbitrary = sized genForgotPasswordResponse

genForgotPasswordResponse :: Int -> Gen ForgotPasswordResponse
genForgotPasswordResponse n =
  ForgotPasswordResponse
    <$> arbitraryReducedMaybe n -- forgotPasswordResponseResetPasswordUrl :: Maybe Text
  
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
    <*> arbitraryReducedMaybe n -- groupType :: Maybe Text
  
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
    <$> arbitraryReducedMaybe n -- groupRuleEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- groupRuleActions :: Maybe GroupRuleAction
    <*> arbitraryReducedMaybe n -- groupRuleAllGroupsValid :: Maybe Bool
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
  
instance Arbitrary HardwareFactor where
  arbitrary = sized genHardwareFactor

genHardwareFactor :: Int -> Gen HardwareFactor
genHardwareFactor n =
  HardwareFactor
    <$> arbitraryReducedMaybe n -- hardwareFactorProfile :: Maybe HardwareFactorProfile
  
instance Arbitrary HardwareFactorProfile where
  arbitrary = sized genHardwareFactorProfile

genHardwareFactorProfile :: Int -> Gen HardwareFactorProfile
genHardwareFactorProfile n =
  HardwareFactorProfile
    <$> arbitraryReducedMaybe n -- hardwareFactorProfileCredentialId :: Maybe Text
  
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
    <*> arbitraryReducedMaybe n -- logAuthenticationContextCredentialProvider :: Maybe [LogCredentialProvider]
    <*> arbitraryReducedMaybe n -- logAuthenticationContextCredentialType :: Maybe [LogCredentialType]
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
  
instance Arbitrary OAuthApplicationCredentials where
  arbitrary = sized genOAuthApplicationCredentials

genOAuthApplicationCredentials :: Int -> Gen OAuthApplicationCredentials
genOAuthApplicationCredentials n =
  OAuthApplicationCredentials
    <$> arbitraryReducedMaybe n -- oAuthApplicationCredentialsOauthClient :: Maybe ApplicationCredentialsOAuthClient
  
instance Arbitrary OpenIdConnectApplication where
  arbitrary = sized genOpenIdConnectApplication

genOpenIdConnectApplication :: Int -> Gen OpenIdConnectApplication
genOpenIdConnectApplication n =
  OpenIdConnectApplication
    <$> arbitraryReducedMaybe n -- openIdConnectApplicationCredentials :: Maybe OAuthApplicationCredentials
    <*> arbitraryReducedMaybeValue n -- openIdConnectApplicationName :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettings :: Maybe OpenIdConnectApplicationSettings
  
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
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientLogoUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientPolicyUri :: Maybe Text
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientRedirectUris :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientResponseTypes :: Maybe [OAuthResponseType]
    <*> arbitraryReducedMaybe n -- openIdConnectApplicationSettingsClientTosUri :: Maybe Text
  
instance Arbitrary PasswordCredential where
  arbitrary = sized genPasswordCredential

genPasswordCredential :: Int -> Gen PasswordCredential
genPasswordCredential n =
  PasswordCredential
    <$> arbitraryReducedMaybe n -- passwordCredentialValue :: Maybe Text
  
instance Arbitrary PushFactor where
  arbitrary = sized genPushFactor

genPushFactor :: Int -> Gen PushFactor
genPushFactor n =
  PushFactor
    <$> arbitraryReducedMaybe n -- pushFactorProfile :: Maybe PushFactorProfile
  
instance Arbitrary PushFactorProfile where
  arbitrary = sized genPushFactorProfile

genPushFactorProfile :: Int -> Gen PushFactorProfile
genPushFactorProfile n =
  PushFactorProfile
    <$> arbitraryReducedMaybe n -- pushFactorProfileCredentialId :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushFactorProfileDeviceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushFactorProfileName :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushFactorProfilePlatform :: Maybe Text
    <*> arbitraryReducedMaybe n -- pushFactorProfileVersion :: Maybe Text
  
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
  
instance Arbitrary Role where
  arbitrary = sized genRole

genRole :: Int -> Gen Role
genRole n =
  Role
    <$> arbitraryReducedMaybe n -- roleEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- roleCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- roleDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- roleId :: Maybe Text
    <*> arbitraryReducedMaybe n -- roleLabel :: Maybe Text
    <*> arbitraryReducedMaybe n -- roleLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- roleStatus :: Maybe RoleStatus
    <*> arbitraryReducedMaybe n -- roleType :: Maybe Text
  
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
    <$> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnAssertionSigned :: Maybe Bool
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
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnRecipient :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnRecipientOverride :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnRequestCompressed :: Maybe Bool
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnResponseSigned :: Maybe Bool
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnSignatureAlgorithm :: Maybe Text
    <*> arbitraryReducedMaybe n -- samlApplicationSettingsSignOnSpIssuer :: Maybe Text
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
    <*> arbitraryReducedMaybe n -- samlAttributeStatementValues :: Maybe [Text]
  
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
  
instance Arbitrary SecurityQuestionFactor where
  arbitrary = sized genSecurityQuestionFactor

genSecurityQuestionFactor :: Int -> Gen SecurityQuestionFactor
genSecurityQuestionFactor n =
  SecurityQuestionFactor
    <$> arbitraryReducedMaybe n -- securityQuestionFactorProfile :: Maybe SecurityQuestionFactorProfile
  
instance Arbitrary SecurityQuestionFactorProfile where
  arbitrary = sized genSecurityQuestionFactorProfile

genSecurityQuestionFactorProfile :: Int -> Gen SecurityQuestionFactorProfile
genSecurityQuestionFactorProfile n =
  SecurityQuestionFactorProfile
    <$> arbitraryReducedMaybe n -- securityQuestionFactorProfileAnswer :: Maybe Text
    <*> arbitraryReducedMaybe n -- securityQuestionFactorProfileQuestion :: Maybe Text
    <*> arbitraryReducedMaybe n -- securityQuestionFactorProfileQuestionText :: Maybe Text
  
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
  
instance Arbitrary SmsFactor where
  arbitrary = sized genSmsFactor

genSmsFactor :: Int -> Gen SmsFactor
genSmsFactor n =
  SmsFactor
    <$> arbitraryReducedMaybe n -- smsFactorProfile :: Maybe SmsFactorProfile
  
instance Arbitrary SmsFactorProfile where
  arbitrary = sized genSmsFactorProfile

genSmsFactorProfile :: Int -> Gen SmsFactorProfile
genSmsFactorProfile n =
  SmsFactorProfile
    <$> arbitraryReducedMaybe n -- smsFactorProfilePhoneNumber :: Maybe Text
  
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
  
instance Arbitrary TokenFactor where
  arbitrary = sized genTokenFactor

genTokenFactor :: Int -> Gen TokenFactor
genTokenFactor n =
  TokenFactor
    <$> arbitraryReducedMaybe n -- tokenFactorProfile :: Maybe TokenFactorProfile
  
instance Arbitrary TokenFactorProfile where
  arbitrary = sized genTokenFactorProfile

genTokenFactorProfile :: Int -> Gen TokenFactorProfile
genTokenFactorProfile n =
  TokenFactorProfile
    <$> arbitraryReducedMaybe n -- tokenFactorProfileCredentialId :: Maybe Text
  
instance Arbitrary TotpFactor where
  arbitrary = sized genTotpFactor

genTotpFactor :: Int -> Gen TotpFactor
genTotpFactor n =
  TotpFactor
    <$> arbitraryReducedMaybe n -- totpFactorProfile :: Maybe TotpFactorProfile
  
instance Arbitrary TotpFactorProfile where
  arbitrary = sized genTotpFactorProfile

genTotpFactorProfile :: Int -> Gen TotpFactorProfile
genTotpFactorProfile n =
  TotpFactorProfile
    <$> arbitraryReducedMaybe n -- totpFactorProfileCredentialId :: Maybe Text
  
instance Arbitrary U2fFactor where
  arbitrary = sized genU2fFactor

genU2fFactor :: Int -> Gen U2fFactor
genU2fFactor n =
  U2fFactor
    <$> arbitraryReducedMaybeValue n -- u2fFactorProfile :: Maybe A.Value
  
instance Arbitrary User where
  arbitrary = sized genUser

genUser :: Int -> Gen User
genUser n =
  User
    <$> arbitraryReducedMaybe n -- userEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- userLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- userActivated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userCreated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userCredentials :: Maybe UserCredentials
    <*> arbitraryReducedMaybe n -- userId :: Maybe Text
    <*> arbitraryReducedMaybe n -- userLastLogin :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userLastUpdated :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userPasswordChanged :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userProfile :: Maybe UserProfile
    <*> arbitraryReducedMaybe n -- userStatus :: Maybe UserStatus
    <*> arbitraryReducedMaybe n -- userStatusChanged :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- userTransitioningToStatus :: Maybe UserStatus
  
instance Arbitrary UserActivationToken where
  arbitrary = sized genUserActivationToken

genUserActivationToken :: Int -> Gen UserActivationToken
genUserActivationToken n =
  UserActivationToken
    <$> arbitraryReducedMaybe n -- userActivationTokenActivationToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- userActivationTokenActivationUrl :: Maybe Text
  
instance Arbitrary UserCredentials where
  arbitrary = sized genUserCredentials

genUserCredentials :: Int -> Gen UserCredentials
genUserCredentials n =
  UserCredentials
    <$> arbitraryReducedMaybe n -- userCredentialsEmails :: Maybe [EmailAddress]
    <*> arbitraryReducedMaybe n -- userCredentialsPassword :: Maybe PasswordCredential
    <*> arbitraryReducedMaybe n -- userCredentialsProvider :: Maybe AuthenticationProvider
    <*> arbitraryReducedMaybe n -- userCredentialsRecoveryQuestion :: Maybe RecoveryQuestionCredential
  
instance Arbitrary UserProfile where
  arbitrary = sized genUserProfile

genUserProfile :: Int -> Gen UserProfile
genUserProfile n =
  UserProfile
    <$> arbitraryReducedMaybe n -- userProfileEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileFirstName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileLastName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileLogin :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileMobilePhone :: Maybe Text
    <*> arbitraryReducedMaybe n -- userProfileSecondEmail :: Maybe Text
  
instance Arbitrary VerifyFactorRequest where
  arbitrary = sized genVerifyFactorRequest

genVerifyFactorRequest :: Int -> Gen VerifyFactorRequest
genVerifyFactorRequest n =
  VerifyFactorRequest
    <$> arbitraryReducedMaybe n -- verifyFactorRequestActivationToken :: Maybe Text
    <*> arbitraryReducedMaybe n -- verifyFactorRequestAnswer :: Maybe Text
    <*> arbitraryReducedMaybe n -- verifyFactorRequestNextPassCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- verifyFactorRequestPassCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- verifyFactorRequestTokenLifetimeSeconds :: Maybe Int
  
instance Arbitrary VerifyFactorResponse where
  arbitrary = sized genVerifyFactorResponse

genVerifyFactorResponse :: Int -> Gen VerifyFactorResponse
genVerifyFactorResponse n =
  VerifyFactorResponse
    <$> arbitraryReducedMaybe n -- verifyFactorResponseEmbedded :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- verifyFactorResponseLinks :: Maybe (Map.Map String A.Value)
    <*> arbitraryReducedMaybe n -- verifyFactorResponseExpiresAt :: Maybe DateTime
    <*> arbitraryReducedMaybe n -- verifyFactorResponseFactorResult :: Maybe FactorResultType
    <*> arbitraryReducedMaybe n -- verifyFactorResponseFactorResultMessage :: Maybe Text
  
instance Arbitrary WebFactor where
  arbitrary = sized genWebFactor

genWebFactor :: Int -> Gen WebFactor
genWebFactor n =
  WebFactor
    <$> arbitraryReducedMaybe n -- webFactorProfile :: Maybe WebFactorProfile
  
instance Arbitrary WebFactorProfile where
  arbitrary = sized genWebFactorProfile

genWebFactorProfile :: Int -> Gen WebFactorProfile
genWebFactorProfile n =
  WebFactorProfile
    <$> arbitraryReducedMaybe n -- webFactorProfileCredentialId :: Maybe Text
  
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
  



instance Arbitrary ApplicationCredentialsScheme where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ApplicationSignOnMode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary AuthenticationProviderType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary EmailStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary EmailType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary FactorProvider where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary FactorResultType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary FactorStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary FactorType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary GroupRuleStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary LogAuthenticationProvider where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary LogCredentialProvider where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary LogCredentialType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary LogSeverity where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OAuthEndpointAuthenticationMethod where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OAuthGrantType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OAuthResponseType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OpenIdConnectApplicationConsentMethod where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OpenIdConnectApplicationType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary RoleStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SessionAuthenticationMethod where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SessionIdentityProviderType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary SessionStatus where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary UserNextLogin where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary UserStatus where
  arbitrary = arbitraryBoundedEnum

