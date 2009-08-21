{-|

\"Blaaargh\!\" is a simple filesystem-based content management system for static or
semi-static publishing. You can run it standalone (...or will be able to soon)
or integrate it as a component in a larger happstack application.

Like the venerable blosxom (<http://blosxom.sourceforge.net/>) package,
Blaaargh\! relies on plain-text files on the file system as its content
database.

FEATURES:

* simple on-disk content database

* posts\/pages written in markdown format

* pages formatted into HTML by a flexible cascading template system based on
  HStringTemplate (<http://hackage.haskell.org/package/HStringTemplate>). Page
  templates can be overridden on a per-page or per-folder basis.

* directories can be given indices using the templating system. Various views
  of the directory contents (e.g. N most recent pages, pages in forward/reverse
  chronological order, pages in alphabetical order) are exposed to the index
  templates.

* directories with defined indices get Atom-format syndication feeds

* configuration parameters (site title, site URL, base URL, etc.) specified in
  an INI-style config file


MISSING FEATURES:

* a web-accessible \"administrative area\"

* a comment system (for my homepage I'm currently just outsourcing this to
  Disqus)



INSTRUCTIONS:

The Blaaargh directory consists of the following contents:

    [@config@]       an INI-style configuration file

    [@templates\/@]  a tree of @.st@ template files, served by HStringTemplate

    [@content\/@]    a tree of content files. Markdown-formatted files (with
                     @.md@ extensions) are parsed and read as \"posts\" that
                     are rendered to HTML via the templating mechanism. Other
                     files are served as static content.


Let's say you have some files in here:

>     config
>     content/grumble.md
>     content/index.md
>     content/posts/bar.md
>     content/posts/bar/photo.jpg
>     content/posts/foo.md
>     content/static/spatula.md
>     templates/404.st
>     templates/post.st
>     templates/static/index.st
>     templates/static/post.st
>     templates/static/spatula.st

Think of it like this -- you request content, either a single post, a
directory, or an atom feed (hardcoded as @\/[dir]\/feed.xml@). Blaaargh
searches for the closest matching template.

For @.md@ files (\"posts\" or \"pages\"), the templates /cascade/: e.g. for a
file @content\/foo\/bar\/baz.md@, we would search @templates\/@ for the following
templates, in order:

* @foo\/bar\/baz.st@

* @foo\/bar\/post.st@

* @foo\/post.st@

* @post.st@

* @404.st@

For directories, the templates don't cascade; a directory needs to have a
matching template in order to be served. E.g. for a directory
@\/foo\/bar\/quux@, we search @templates\/@ for \"@foo\/bar\/quux\/index.st@\",
and if @content\/foo\/bar\/quux\/index.md@ exists, we read it into the template
as content text. (More about this in \"TEMPLATING\" below.)


CONFIGURATION

A Blaaargh\! @config@ file looks like this:

> [default]
> # what's the domain name?
> siteurl = http://example.com
>
> # blaaargh content will be served at this base URL
> baseurl = /foo
>
> [feed]
> # site title
> title    = Example dot com
> # authors
> authors  = John Smith <john@example.com>
> # Atom icon
> icon     = /static/icon.png
>
> # posts on this list (or directories containing posts) won't be included in
> # directory indices, nor in atom feeds
> skipurls = static

Blaaargh\! uses the ConfigFile library for configuration and post header
parsing. (<http://hackage.haskell.org/package/ConfigFile>)


POST FORMATTING

Posts are (mostly) in Markdown format, with the exception of a key-value header
prefixed by pipe (@\|@) characters. Example:

> | title: Example post
> | author: John Smith <john@example.com>
> | published: 2009-09-15T21:18:00-0400
> | summary: A short summary of the post contents
> 
> This is an example post in *markdown* format.
> 

Blaaargh\! accepts the following key-values for posts:

  [@title@] The title of the post

  [@author@] The post's author

  [@authors@] same as @author@

  [@summary@] a summary of the post

  [@updated@] the post's last update time in RFC3339 format

  [@published@] the post's publish date in RFC3339 format

The headers are parsed as follows: any lines starting with the @\|@ character
(up until the first line not starting with @\|@) have the prefix stripped and
are sent through ConfigFile.

Please see its haddock for input syntax.


DATA MODEL

The documents get indexed into a key-value mapping by id. (Where an \"id\" for
a post is defined as its relative path from @content\/@, with the @.md@ suffix
removed.)

Files with an @\'.md\'@ suffix are treated as \"posts\"\/\"pages\", and are
expected to be in markdown format. Files with other suffices are served as
static files.


TEMPLATING

Blaaargh\! uses templates to present the content of posts and lists of posts in
HTML.

For an individual post (either postname-specific @/postname/.st@ or generic
@post.st@), Blaaargh exports a template variable called @$post$@ which is a map
containing the following attributes:

@
    $id$
    $date$
    $url$
    $title$
    $content$
    $summary$
    $authors$
@

So in other words, within your template the post's URL can be accessed using
@$post.id$@.

For directory templates (@index.st@), we collect the posts within that
directory and present them to the templating system as a list of post objects
(i.e. containing the @$id$@\/@$date$@\/etc. fields listed above):

@
    $alphabeticalPosts$
    $recentPosts$                -- N.B. 5 most recent posts
    $chronologicalPosts$
    $reverseChronologicalPosts$
@

-}


module Blaaargh
  ( initBlaaargh
  , serveBlaaargh
  , runBlaaarghHandler
  , addExtraTemplateArguments
  , BlaaarghException
  , blaaarghExceptionMsg
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
import           Blaaargh.Internal.Handlers
import           Blaaargh.Internal.Post
import           Blaaargh.Internal.Types
import qualified Blaaargh.Internal.Util.ExcludeList as EL
import           Blaaargh.Internal.Util.ExcludeList (ExcludeList)
import           Blaaargh.Internal.Util.Templates


------------------------------------------------------------------------------
{-|

  Initialize a Blaaargh instance. Given the name of a directory on the disk,
  'initBlaaargh' searches it for configuration, content, and template files,
  and produces a finished 'BlaaarghState' value. Throws a 'BlaaarghException'
  if there was an error reading the state.

-}
initBlaaargh :: FilePath        -- ^ path to blaaargh directory
             -> IO BlaaarghState
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


------------------------------------------------------------------------------

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
