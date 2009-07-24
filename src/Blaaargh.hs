module Blaaargh
  ( initBlaaargh
  , serveBlaaargh
  , runBlaaarghHandler
  , addExtraTemplateArguments
  , BlaaarghException
  , BlaaarghMonad
  , BlaaarghHandler
  , BlaaarghState
  )
where

------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import           Data.Char
import qualified Data.ConfigFile as Cfg
import           Data.Either
import           Data.List
import           System.Directory
import           System.FilePath
import qualified Text.Atom.Feed as Atom
import           Text.Printf

------------------------------------------------------------------------------
import           Blaaargh.Handlers
import           Blaaargh.Post
import           Blaaargh.Types
import qualified Blaaargh.Util.ExcludeList as EL
import           Blaaargh.Util.ExcludeList (ExcludeList)
import           Blaaargh.Util.Templates


------------------------------------------------------------------------------
initBlaaargh :: FilePath -> IO BlaaarghState
initBlaaargh path = do
    -- make sure directories exist
    mapM_ failIfNotDir [path, contentDir, templateDir]

    (feed, siteURL, baseURL, excludeList) <- readConfig configFilePath

    cmap      <- buildContentMap baseURL contentDir
    templates <- readTemplateDir templateDir

    return BlaaarghState {
                      blaaarghPath          = path
                    , blaaarghSiteURL       = siteURL
                    , blaaarghBaseURL       = baseURL
                    , blaaarghPostMap       = cmap
                    , blaaarghTemplates     = templates
                    , blaaarghFeedInfo      = feed
                    , blaaarghFeedExcludes  = excludeList
                    , blaaarghExtraTmpl     = id
                    }

  where
    --------------------------------------------------------------------------
    unlessM :: IO Bool -> IO () -> IO ()
    unlessM b act = b >>= flip unless act

    --------------------------------------------------------------------------
    failIfNotDir :: FilePath -> IO ()
    failIfNotDir d = unlessM (doesDirectoryExist d)
                             (throwIO $ BlaaarghException
                                      $ printf "'%s' is not a directory" path)

    --------------------------------------------------------------------------
    configFilePath = path </> "config"
    contentDir     = path </> "content"
    templateDir    = path </> "templates"


getM :: Cfg.Get_C a => Cfg.ConfigParser -> String -> String -> Maybe a
getM cp section = either (const Nothing) Just . Cfg.get cp section


readConfig :: FilePath -> IO (Atom.Feed, String, String, ExcludeList)
readConfig fp = do
    cp <- parseConfig fp

    either (throwIO . BlaaarghException . show)
           return
           (mkFeed cp)
  where
    ensurePrefix :: Char -> String -> String
    ensurePrefix p s = if [p] `isPrefixOf` s then s else p:s

    stripSuffix :: Char -> String -> String
    stripSuffix x s = if [x] `isSuffixOf` s then init s else s


    mkFeed :: Either Cfg.CPError Cfg.ConfigParser
           -> Either Cfg.CPError (Atom.Feed, String, String, ExcludeList)
    mkFeed cfg = do
      cp       <- cfg
      title    <- Cfg.get cp "feed" "title"
      authors  <- Cfg.get cp "feed" "authors"
      baseURL' <- Cfg.get cp "default" "baseurl"
      siteURL' <- Cfg.get cp "default" "siteurl"

      let icon = getM cp "feed" "icon"
      let skip = maybe EL.empty
                       (EL.fromPathList . B.pack)
                       (getM cp "feed" "skipurls")

      let siteURL = stripSuffix '/' siteURL'
      let baseURL = stripSuffix '/' $ ensurePrefix '/' baseURL'
      let feedURL = (siteURL ++ baseURL)

      let feed = Atom.nullFeed feedURL
                               (Atom.TextString title)
                               ""

      let feed' = feed { Atom.feedAuthors = parsePersons authors
                       , Atom.feedIcon    = icon
                       , Atom.feedLinks   = [ Atom.nullLink feedURL ]
                       }

      return (feed', siteURL, baseURL, skip)


parseConfig :: FilePath -> IO (Either Cfg.CPError Cfg.ConfigParser)
parseConfig = Cfg.readfile Cfg.emptyCP


