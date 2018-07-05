{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- | API definition for mass-driver.
module MassDriver.API
  ( API
  , api
  , APIConfig
  , flags
  , server
  , app
  ) where

import Protolude

import Lucid
import Network.URI (URI(..))
import qualified Network.URI as URI
import qualified Options.Applicative as Opt
import qualified Servant
import Servant.API (Get, (:<|>)(..), (:>), Raw)
import Servant.HTML.Lucid (HTML)
import Servant.Server (hoistServer)
import Servant.Utils.StaticFiles (serveDirectoryWebApp)

import qualified MassDriver.Terraform as Terraform

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
    upstreamURL' = "https://github.com/jml/mass-driver"

-- | Configuration from the whole application, used in various handlers.
data Config
  = Config
  { apiConfig :: APIConfig
  , terraformConfig :: Terraform.Config
  } deriving (Eq, Show)

-- | mass-driver API definition.
type API
  = "api" :> "plan" :> Get '[HTML] (Page TerraformPlan)
  :<|> StaticResources
  :<|> Get '[HTML] (Page Root)

-- | Where all of our stylesheets live.
type StaticResources = "static" :> Raw

-- | Value-level representation of 'API'.
api :: Proxy API
api = Proxy

-- | WAI application that implements 'API'.
app :: APIConfig -> Terraform.Config -> Servant.Application
app apiConfig terraformConfig = Servant.serve api (server (Config apiConfig terraformConfig))

-- | Server-side implementation of 'API'.
server :: Config -> Servant.Server API
server config = hoistServer api (`runReaderT` config)
  ( loadTerraformPlan
    :<|> serveDirectoryWebApp (staticDir (apiConfig config))
    :<|> makePage "mass-driver" Root
  )

-- | Dummy type to represent the root page of the API.
data Root = Root deriving (Eq, Show)

instance ToHtml Root where
  toHtmlRaw = toHtml
  toHtml _ =
    div_ [class_ "jumbotron"] $
      div_ [class_ "container"] $ do
        h1_ [class_ "display-3"] "mass-driver"
        p_ "Automatically apply Terraform configurations for great impact."

-- | The results of @terraform plan@.
--
-- Although we'll probably always want a type for this, it will probably look
-- quite different in the future.
data TerraformPlan
  = TerraformPlan
  { initResult :: ProcessResult
  , planResult :: ProcessResult
  } deriving (Eq, Show)

instance ToHtml TerraformPlan where
  toHtmlRaw = toHtml
  toHtml TerraformPlan{initResult, planResult} =
    div_ [class_ "container"] $ do
      h1_ "terraform plan"
      toHtml initResult
      toHtml planResult

-- | The result of running a process. Includes a field for describing the
-- process to make HTML rendering easier.
data ProcessResult
  = ProcessResult
  { processTitle :: Text
  , processExitCode :: ExitCode
  , processOutput :: ByteString
  , processError :: ByteString
  } deriving (Eq, Show)

instance ToHtml ProcessResult where
  toHtmlRaw = toHtml
  toHtml ProcessResult{processTitle, processExitCode, processOutput, processError} =
    div_ [class_ "container"] $ do
      h2_ (toHtml processTitle)
      dl_ $ do
        dt_ "Exit code"
        dd_ (toHtml (show processExitCode :: Text))
        dt_ "stdout"
        dd_ $ pre_ (toHtml processOutput)
        dt_ "stderr"
        dd_ $ pre_ (toHtml processError)


loadTerraformPlan :: ReaderT Config Servant.Handler (Page TerraformPlan)
loadTerraformPlan = do
  Config{terraformConfig} <- ask
  -- TODO: Better error control. Don't run 'plan' if 'init' fails.
  initResult <- runProcess "init" $ Terraform.init terraformConfig
  planResult <- runProcess "plan" $ Terraform.plan terraformConfig
  makePage "terraform plan" $ TerraformPlan initResult planResult
  where
    runProcess name action = do
      (exitCode, out, err) <- liftIO action
      pure $ ProcessResult name exitCode out err

-- | A standard HTML page in the mass-driver app.
data Page a
  = Page
  { config :: APIConfig -- ^ The configuration for the app
  , title :: Text  -- ^ The title of the page
  , content :: a  -- ^ The main content
  } deriving (Eq, Show)

instance ToHtml a => ToHtml (Page a) where
  toHtmlRaw = toHtml
  toHtml Page{config, title, content} =
    doctypehtml_ $ do
      head_ $ do
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
        link_ [rel_ "stylesheet", staticLink config "style.css"]
        title_ (toHtml title)
      body_ $ do
        main_ [role_ "main"] (toHtml content)
        footer_ [class_ "container"] $
          p_ $ do
            "Copyright © Jonathan M. Lange 2018. "
            "Made available under the "
            a_ [href_ "https://www.gnu.org/licenses/agpl.html"] "AGPLv3 license"
            ". "
            "Source code at "
            a_ [uriHref_ (sourceURL config)] (toHtml (show @URI @Text (sourceURL config)))
            "."

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
makePage :: MonadReader Config m => Text -> body -> m (Page body)
makePage title body = do
  Config{apiConfig} <- ask
  pure (Page apiConfig title body)
