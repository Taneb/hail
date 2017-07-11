{-# LANGUAGE OverloadedStrings #-}
module Hail.Hydra where

import Control.Exception (handleJust)

import Network.Wreq
import Network.URI
import Network.Connection (TLSSettings(TLSSettings))
import Network.HTTP.Client (HttpException (InvalidUrlException))
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.TLS (clientSupported, defaultParamsClient, supportedCiphers)
import Network.TLS.Extra (ciphersuite_default)
import Data.Aeson.Lens
import Control.Lens.Operators
import Data.Default.Class (def)
import Data.Maybe (fromJust)
import Data.Text (unpack)

-- | Get the out path of the latest build of a given hydra job.
getLatest
  :: String     -- ^ The URI to the hydra job.
  -> Maybe Auth -- ^ The credentials to talk to hydra.
  -> IO (Either String FilePath)
getLatest uri m_creds =
    handleJust isTransientError (return . Left . (show :: HttpException -> String)) $ do
      r <- getWith opts latestUri
      let m_outPath = r ^? responseBody . key "buildoutputs"
                                        . key "out"
                                        . key "path" . _String
      return $ case m_outPath of
        Nothing -> Left noOutErr
        Just outPath -> Right $ unpack outPath
  where
    opts = defaults & header "Accept" .~ ["application/json"]
                    & auth .~ m_creds
                    & manager .~ Left (mkManagerSettings (TLSSettings clientParams) Nothing)

    hostname = uriRegName . fromJust .
               uriAuthority . fromJust $
               parseURI uri

    clientParams = (defaultParamsClient hostname "") {
      clientSupported = def {supportedCiphers = ciphersuite_default}
      }

    noOutErr =
      "Latest build of job " ++ uri
                             ++ " has no output named 'out'."
    latestUri = uri ++ "/latest"

    isTransientError e = case e of
      InvalidUrlException _ _ -> Nothing
      _ -> Just e
