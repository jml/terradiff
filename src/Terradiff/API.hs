{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- | API definition for terradiff.
module Terradiff.API
  ( API
  , api
  , APIConfig
  , flags
  , server
  , app
  ) where

import Protolude

import Lucid
import Network.URI (URI(..), relativeTo)
import qualified Network.URI as URI
import qualified Options.Applicative as Opt
import qualified Servant
import Servant.API (Get, (:<|>)(..), (:>), Raw)
import Servant.HTML.Lucid (HTML)
import Servant.Server (hoistServer)
import Servant.Utils.Links (Link, linkURI, safeLink)
import Servant.Utils.StaticFiles (serveDirectoryWebApp, serveDirectoryFileServer)

import Terradiff.Poll (Poll)
import qualified Terradiff.Poll as Poll
import qualified Terradiff.Terraform as Terraform

-- | Configuration for the API. Mostly used for the HTML rendering.
data APIConfig
  = APIConfig
  { externalURL :: URI  -- ^ Publicly visible base URL of the service, used for making links
  , staticDir :: FilePath  -- ^ Directory containing static resources
  , sourceURL :: URI -- ^ Where the source code for the API server lives
  } deriving (Eq, Show)

-- | Configure the API via the command-line.
flags :: Opt.Parser APIConfig
flags =
  APIConfig
  <$> Opt.option
        (Opt.eitherReader parseURI)
        (fold
           [ Opt.long "external-url"
           , Opt.help "Publicly visible base URL of the service."
           ])
  <*> Opt.option
        Opt.str
        (fold
          [ Opt.long "static-dir"
          , Opt.help "Path to directory containing static resources."
          ])
  <*> Opt.option
        (Opt.eitherReader parseURI)
        (fold
          [ Opt.long "source-code-url"
          , Opt.help "Where users can find the source code for the server."
          , Opt.value upstreamURL
          ])
  where
    parseURI = note "Must be an absolute URL" . URI.parseAbsoluteURI

    upstreamURL = fromMaybe (panic ("Invalid source code URI in source code: " <> toS upstreamURL')) (URI.parseAbsoluteURI upstreamURL')
    upstreamURL' = "https://github.com/jml/terradiff"

-- | Configuration from the whole application, used in various handlers.
data Config
  = Config
  { apiConfig :: APIConfig
  , terraformConfigPath :: FilePath
  , planPoller :: Poll PlanResult
  }

-- | terradiff API definition.
type API
  = TerraformConfig
    :<|> StaticResources
    :<|> DiffPage

-- | The diff itself!
type DiffPage = Get '[HTML] (Page TerraformPlan)

-- | Where all of our stylesheets live.
type StaticResources = "static" :> Raw

-- | The actual Terraform configuration files.
type TerraformConfig = "terraform-config" :> Raw

-- | Value-level representation of 'API'.
api :: Proxy API
api = Proxy

-- | WAI application that implements 'API'.
app :: APIConfig -> FilePath -> Poll PlanResult -> Servant.Application
app apiConfig terraformConfigPath terraformConfig = Servant.serve api (server (Config apiConfig terraformConfigPath terraformConfig))

-- | Server-side implementation of 'API'.
server :: Config -> Servant.Server API
server config = hoistServer api (`runReaderT` config)
  ( serveDirectoryFileServer (terraformConfigPath config)
    :<|> serveDirectoryWebApp (staticDir (apiConfig config))
    :<|> viewTerraformPlan
  )

viewTerraformPlan :: ReaderT Config Servant.Handler (Page TerraformPlan)
viewTerraformPlan = do
  Config{planPoller} <- ask
  plan <- liftIO (atomically (Poll.waitForResult planPoller))
  makePage "terradiff" (safeLink api (Proxy @DiffPage)) (TerraformPlan plan)

-- | Results of running @terraform plan@. This is what we get out of the poller.
type PlanResult = Either Terraform.Error (Maybe Terraform.Diff)

-- | Simple wrapper for 'Plan' type so we can have all our HTML in one place.
newtype TerraformPlan = TerraformPlan PlanResult deriving (Eq, Show)

instance ToHtml TerraformPlan where
  toHtmlRaw = toHtml
  toHtml (TerraformPlan planResult) =
    div_ [class_ "container mt-2"] $ do
      h1_ "terradiff"
      case planResult of
        Left (Terraform.ProcessError processResult) -> do
          div_ [class_ "alert alert-danger", role_ "alert"] $ do
            "There was an error running "
            code_ "terraform"
          processToHtml processResult
        Right (Just (Terraform.Diff diffOutput)) -> do
          div_ [class_ "alert alert-warning", role_ "alert"]
            "Configuration and environment do not match"
          pre_ [class_ "ml-2"] (code_ (toHtml diffOutput))
        Right Nothing ->
          div_ [class_ "alert alert-success", role_ "alert"]
            "Configuration and environment are up-to-date"
    where
      processToHtml :: Monad m => Terraform.ProcessResult -> HtmlT m ()
      processToHtml Terraform.ProcessResult { Terraform.processTitle
                                            , Terraform.processExitCode
                                            , Terraform.processOutput
                                            , Terraform.processError
                                            } = do
          h2_ "Details"
          dl_ $ do
            dt_ "Command"
            dd_ $ code_ (toHtml processTitle)
            dt_ "Exit code"
            dd_ (toHtml (show processExitCode :: Text))
            dt_ "stdout"
            dd_ $ pre_ [class_ "ml-2"] (toHtml processOutput)
            dt_ "stderr"
            dd_ $ pre_ [class_ "ml-2"] (toHtml processError)

-- | A standard HTML page in the terradiff app.
data Page a
  = Page
  { config :: APIConfig -- ^ The configuration for the app
  , title :: Text  -- ^ The title of the page
  , currentURI :: URI   -- ^ URI of the page the user is currently viewing
  , content :: a  -- ^ The main content
  } deriving (Eq, Show)

instance ToHtml a => ToHtml (Page a) where
  toHtmlRaw = toHtml
  toHtml Page{config, title, currentURI, content} =
    doctypehtml_ $ do
      head_ $ do
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
        link_ [rel_ "stylesheet", staticLink config "style.css"]
        title_ (toHtml title)
      body_ $ do
        nav_ [class_ "navbar navbar-expand-lg navbar-dark bg-dark"] $
          div_ [class_ "collapse navbar-collapse"] $
            ul_ [class_ "navbar-nav mr-auto"] $ do
              let baseURL = externalURL config
              forM_ navLinks $ \(link', label) ->
                let destURL = safeHref_ baseURL link'
                in if linkURI link' == currentURI
                   then
                     li_ [class_ "nav-item active"] $
                     a_ [class_ "nav-link", destURL] $ do
                       label
                       " "
                       span_ [class_ "sr-only"] "(current)"
                   else li_ [class_ "nav-item"] $ a_ [class_ "nav-link", destURL] label
        main_ [role_ "main"] (toHtml content)
        footer_ [class_ "container"] $ do
          hr_ empty
          p_ [class_ "font-weight-light"] $ do
            "Copyright © Jonathan M. Lange 2018. "
            "Made available under the "
            a_ [href_ "https://www.gnu.org/licenses/agpl.html"] "AGPLv3 license"
            ". "
            "Source code at "
            a_ [uriHref_ (sourceURL config)] (toHtml (show @URI @Text (sourceURL config)))
            "."
    where
      navLinks =
        [ (safeLink api (Proxy @DiffPage), "Diff")
        , (safeLink api (Proxy @TerraformConfig), "Configuration")
        ]

-- | Generate an href= attribute that links to something in the API, relative
-- to the external URL.
safeHref_ :: URI -> Link -> Attribute
safeHref_ externalURL link' = href_ (show (linkURI link' `relativeTo` externalURL))

-- | Link to a resource within our static resources.
--
-- e.g. staticLink config "style.css"
staticLink :: APIConfig -> Text -> Attribute
staticLink APIConfig{externalURL} resourcePath =
  Servant.safeLink' (uriHref_ . addSuffix . makeAbsolute) api (Proxy @StaticResources)
  where
    makeAbsolute link' = Servant.linkURI link' `URI.relativeTo` externalURL
    addSuffix uri = uri { uriPath = uriPath uri <> "/" <> toS resourcePath }

-- | Make an href=<uri> attribute.
uriHref_ :: URI -> Attribute
uriHref_ uri = href_ (show uri)

-- | Make a standard HTML page in the compare revisions app.
makePage :: MonadReader Config m => Text -> Link -> body -> m (Page body)
makePage title currentLink body = do
  Config{apiConfig} <- ask
  pure (Page apiConfig title (linkURI currentLink) body)
