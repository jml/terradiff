{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- | API definition for mass-driver.
module MassDriver.API
  ( API
  , api
  , Config
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

-- | Configuration for the API. Mostly used for the HTML rendering.
data Config
  = Config
  { externalURL :: URI  -- ^ Publicly visible base URL of the service, used for making links
  , staticDir :: FilePath  -- ^ Directory containing static resources
  , sourceURL :: URI -- ^ Where the source code for the API server lives
  } deriving (Eq, Show)

-- | Configure the API via the command-line.
flags :: Opt.Parser Config
flags =
  Config
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


-- | mass-driver API definition.
type API
  = StaticResources
  :<|> Get '[HTML] (Page Root)

-- | Where all of our stylesheets live.
type StaticResources = "static" :> Raw

-- | Value-level representation of 'API'.
api :: Proxy API
api = Proxy

-- | WAI application that implements 'API'.
app :: Config -> Servant.Application
app config = Servant.serve api (server config)

-- | Server-side implementation of 'API'.
server :: Config -> Servant.Server API
server config = hoistServer api (`runReaderT` config)
  ( serveDirectoryWebApp (staticDir config)
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

-- | A standard HTML page in the mass-driver app.
data Page a
  = Page
  { config :: Config -- ^ The configuration for the app
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
staticLink :: Config -> Text -> Attribute
staticLink Config{externalURL} resourcePath =
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
  config <- ask
  pure (Page config title body)
