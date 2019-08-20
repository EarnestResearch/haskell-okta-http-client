{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import Okta.Model
import Okta.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy AppLink)
      propMimeEq MimeJSON (Proxy :: Proxy AppUser)
      propMimeEq MimeJSON (Proxy :: Proxy AppUserCredentials)
      propMimeEq MimeJSON (Proxy :: Proxy AppUserPasswordCredential)
      propMimeEq MimeJSON (Proxy :: Proxy Application)
      propMimeEq MimeJSON (Proxy :: Proxy ApplicationAccessibility)
      propMimeEq MimeJSON (Proxy :: Proxy ApplicationCredentials)
      propMimeEq MimeJSON (Proxy :: Proxy ApplicationCredentialsOAuthClient)
      propMimeEq MimeJSON (Proxy :: Proxy ApplicationCredentialsScheme)
      propMimeEq MimeJSON (Proxy :: Proxy ApplicationCredentialsSigning)
      propMimeEq MimeJSON (Proxy :: Proxy ApplicationCredentialsUsernameTemplate)
      propMimeEq MimeJSON (Proxy :: Proxy ApplicationGroupAssignment)
      propMimeEq MimeJSON (Proxy :: Proxy ApplicationLicensing)
      propMimeEq MimeJSON (Proxy :: Proxy ApplicationSettings)
      propMimeEq MimeJSON (Proxy :: Proxy ApplicationSettingsNotifications)
      propMimeEq MimeJSON (Proxy :: Proxy ApplicationSettingsNotificationsVpn)
      propMimeEq MimeJSON (Proxy :: Proxy ApplicationSettingsNotificationsVpnNetwork)
      propMimeEq MimeJSON (Proxy :: Proxy ApplicationSignOnMode)
      propMimeEq MimeJSON (Proxy :: Proxy ApplicationVisibility)
      propMimeEq MimeJSON (Proxy :: Proxy ApplicationVisibilityHide)
      propMimeEq MimeJSON (Proxy :: Proxy AuthenticationProvider)
      propMimeEq MimeJSON (Proxy :: Proxy AuthenticationProviderType)
      propMimeEq MimeJSON (Proxy :: Proxy AutoLoginApplication)
      propMimeEq MimeJSON (Proxy :: Proxy AutoLoginApplicationSettings)
      propMimeEq MimeJSON (Proxy :: Proxy AutoLoginApplicationSettingsSignOn)
      propMimeEq MimeJSON (Proxy :: Proxy BasicApplicationSettings)
      propMimeEq MimeJSON (Proxy :: Proxy BasicApplicationSettingsApplication)
      propMimeEq MimeJSON (Proxy :: Proxy BasicAuthApplication)
      propMimeEq MimeJSON (Proxy :: Proxy BookmarkApplication)
      propMimeEq MimeJSON (Proxy :: Proxy BookmarkApplicationSettings)
      propMimeEq MimeJSON (Proxy :: Proxy BookmarkApplicationSettingsApplication)
      propMimeEq MimeJSON (Proxy :: Proxy BrowserPluginApplication)
      propMimeEq MimeJSON (Proxy :: Proxy CallFactor)
      propMimeEq MimeJSON (Proxy :: Proxy CallFactorProfile)
      propMimeEq MimeJSON (Proxy :: Proxy ChangePasswordRequest)
      propMimeEq MimeJSON (Proxy :: Proxy CreateSessionRequest)
      propMimeEq MimeJSON (Proxy :: Proxy EmailAddress)
      propMimeEq MimeJSON (Proxy :: Proxy EmailFactor)
      propMimeEq MimeJSON (Proxy :: Proxy EmailFactorProfile)
      propMimeEq MimeJSON (Proxy :: Proxy EmailStatus)
      propMimeEq MimeJSON (Proxy :: Proxy EmailType)
      propMimeEq MimeJSON (Proxy :: Proxy Factor)
      propMimeEq MimeJSON (Proxy :: Proxy FactorProvider)
      propMimeEq MimeJSON (Proxy :: Proxy FactorResultType)
      propMimeEq MimeJSON (Proxy :: Proxy FactorStatus)
      propMimeEq MimeJSON (Proxy :: Proxy FactorType)
      propMimeEq MimeJSON (Proxy :: Proxy ForgotPasswordResponse)
      propMimeEq MimeJSON (Proxy :: Proxy Group)
      propMimeEq MimeJSON (Proxy :: Proxy GroupProfile)
      propMimeEq MimeJSON (Proxy :: Proxy GroupRule)
      propMimeEq MimeJSON (Proxy :: Proxy GroupRuleAction)
      propMimeEq MimeJSON (Proxy :: Proxy GroupRuleConditions)
      propMimeEq MimeJSON (Proxy :: Proxy GroupRuleExpression)
      propMimeEq MimeJSON (Proxy :: Proxy GroupRuleGroupAssignment)
      propMimeEq MimeJSON (Proxy :: Proxy GroupRuleGroupCondition)
      propMimeEq MimeJSON (Proxy :: Proxy GroupRulePeopleCondition)
      propMimeEq MimeJSON (Proxy :: Proxy GroupRuleStatus)
      propMimeEq MimeJSON (Proxy :: Proxy GroupRuleUserCondition)
      propMimeEq MimeJSON (Proxy :: Proxy HardwareFactor)
      propMimeEq MimeJSON (Proxy :: Proxy HardwareFactorProfile)
      propMimeEq MimeJSON (Proxy :: Proxy JsonWebKey)
      propMimeEq MimeJSON (Proxy :: Proxy LogActor)
      propMimeEq MimeJSON (Proxy :: Proxy LogAuthenticationContext)
      propMimeEq MimeJSON (Proxy :: Proxy LogAuthenticationProvider)
      propMimeEq MimeJSON (Proxy :: Proxy LogClient)
      propMimeEq MimeJSON (Proxy :: Proxy LogCredentialProvider)
      propMimeEq MimeJSON (Proxy :: Proxy LogCredentialType)
      propMimeEq MimeJSON (Proxy :: Proxy LogDebugContext)
      propMimeEq MimeJSON (Proxy :: Proxy LogEvent)
      propMimeEq MimeJSON (Proxy :: Proxy LogGeographicalContext)
      propMimeEq MimeJSON (Proxy :: Proxy LogGeolocation)
      propMimeEq MimeJSON (Proxy :: Proxy LogIpAddress)
      propMimeEq MimeJSON (Proxy :: Proxy LogIssuer)
      propMimeEq MimeJSON (Proxy :: Proxy LogOutcome)
      propMimeEq MimeJSON (Proxy :: Proxy LogRequest)
      propMimeEq MimeJSON (Proxy :: Proxy LogSecurityContext)
      propMimeEq MimeJSON (Proxy :: Proxy LogSeverity)
      propMimeEq MimeJSON (Proxy :: Proxy LogTarget)
      propMimeEq MimeJSON (Proxy :: Proxy LogTransaction)
      propMimeEq MimeJSON (Proxy :: Proxy LogUserAgent)
      propMimeEq MimeJSON (Proxy :: Proxy OAuthApplicationCredentials)
      propMimeEq MimeJSON (Proxy :: Proxy OAuthEndpointAuthenticationMethod)
      propMimeEq MimeJSON (Proxy :: Proxy OAuthGrantType)
      propMimeEq MimeJSON (Proxy :: Proxy OAuthResponseType)
      propMimeEq MimeJSON (Proxy :: Proxy OpenIdConnectApplication)
      propMimeEq MimeJSON (Proxy :: Proxy OpenIdConnectApplicationConsentMethod)
      propMimeEq MimeJSON (Proxy :: Proxy OpenIdConnectApplicationSettings)
      propMimeEq MimeJSON (Proxy :: Proxy OpenIdConnectApplicationSettingsClient)
      propMimeEq MimeJSON (Proxy :: Proxy OpenIdConnectApplicationType)
      propMimeEq MimeJSON (Proxy :: Proxy PasswordCredential)
      propMimeEq MimeJSON (Proxy :: Proxy PushFactor)
      propMimeEq MimeJSON (Proxy :: Proxy PushFactorProfile)
      propMimeEq MimeJSON (Proxy :: Proxy RecoveryQuestionCredential)
      propMimeEq MimeJSON (Proxy :: Proxy ResetPasswordToken)
      propMimeEq MimeJSON (Proxy :: Proxy Role)
      propMimeEq MimeJSON (Proxy :: Proxy RoleStatus)
      propMimeEq MimeJSON (Proxy :: Proxy SamlApplication)
      propMimeEq MimeJSON (Proxy :: Proxy SamlApplicationSettings)
      propMimeEq MimeJSON (Proxy :: Proxy SamlApplicationSettingsSignOn)
      propMimeEq MimeJSON (Proxy :: Proxy SamlAttributeStatement)
      propMimeEq MimeJSON (Proxy :: Proxy SchemeApplicationCredentials)
      propMimeEq MimeJSON (Proxy :: Proxy SecurePasswordStoreApplication)
      propMimeEq MimeJSON (Proxy :: Proxy SecurePasswordStoreApplicationSettings)
      propMimeEq MimeJSON (Proxy :: Proxy SecurePasswordStoreApplicationSettingsApplication)
      propMimeEq MimeJSON (Proxy :: Proxy SecurityQuestion)
      propMimeEq MimeJSON (Proxy :: Proxy SecurityQuestionFactor)
      propMimeEq MimeJSON (Proxy :: Proxy SecurityQuestionFactorProfile)
      propMimeEq MimeJSON (Proxy :: Proxy Session)
      propMimeEq MimeJSON (Proxy :: Proxy SessionAuthenticationMethod)
      propMimeEq MimeJSON (Proxy :: Proxy SessionIdentityProvider)
      propMimeEq MimeJSON (Proxy :: Proxy SessionIdentityProviderType)
      propMimeEq MimeJSON (Proxy :: Proxy SessionStatus)
      propMimeEq MimeJSON (Proxy :: Proxy SmsFactor)
      propMimeEq MimeJSON (Proxy :: Proxy SmsFactorProfile)
      propMimeEq MimeJSON (Proxy :: Proxy SwaApplication)
      propMimeEq MimeJSON (Proxy :: Proxy SwaApplicationSettings)
      propMimeEq MimeJSON (Proxy :: Proxy SwaApplicationSettingsApplication)
      propMimeEq MimeJSON (Proxy :: Proxy SwaThreeFieldApplication)
      propMimeEq MimeJSON (Proxy :: Proxy SwaThreeFieldApplicationSettings)
      propMimeEq MimeJSON (Proxy :: Proxy SwaThreeFieldApplicationSettingsApplication)
      propMimeEq MimeJSON (Proxy :: Proxy TempPassword)
      propMimeEq MimeJSON (Proxy :: Proxy TokenFactor)
      propMimeEq MimeJSON (Proxy :: Proxy TokenFactorProfile)
      propMimeEq MimeJSON (Proxy :: Proxy TotpFactor)
      propMimeEq MimeJSON (Proxy :: Proxy TotpFactorProfile)
      propMimeEq MimeJSON (Proxy :: Proxy U2fFactor)
      propMimeEq MimeJSON (Proxy :: Proxy User')
      propMimeEq MimeJSON (Proxy :: Proxy UserActivationToken)
      propMimeEq MimeJSON (Proxy :: Proxy UserCredentials)
      propMimeEq MimeJSON (Proxy :: Proxy UserNextLogin)
      propMimeEq MimeJSON (Proxy :: Proxy UserProfile)
      propMimeEq MimeJSON (Proxy :: Proxy UserStatus)
      propMimeEq MimeJSON (Proxy :: Proxy VerifyFactorRequest)
      propMimeEq MimeJSON (Proxy :: Proxy VerifyFactorResponse)
      propMimeEq MimeJSON (Proxy :: Proxy WebFactor)
      propMimeEq MimeJSON (Proxy :: Proxy WebFactorProfile)
      propMimeEq MimeJSON (Proxy :: Proxy WsFederationApplication)
      propMimeEq MimeJSON (Proxy :: Proxy WsFederationApplicationSettings)
      propMimeEq MimeJSON (Proxy :: Proxy WsFederationApplicationSettingsApplication)
      
