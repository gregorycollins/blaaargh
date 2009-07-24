{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Blaaargh.Types 
  ( module Blaaargh.Exception
  , Post(..)
  , getPostTime
  , ContentMap
  , ContentItem(..)
  , BlaaarghState(..)
  , BlaaarghMonad
  , BlaaarghHandler
  , liftB
  , runBlaaarghHandler
  , addExtraTemplateArguments
  )
where

------------------------------------------------------------------------------
import           Control.Monad.State
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe
import           Data.Time.LocalTime
import           Happstack.Server
import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Atom
import           Text.StringTemplate
import           Text.StringTemplate.Classes
import qualified Text.XML.Light.Output as XML
------------------------------------------------------------------------------
import           Blaaargh.Exception
import           Blaaargh.Time
import           Blaaargh.Util.ExcludeList
import           Blaaargh.Util.Templates

------------------------------------------------------------------------------
--
--  .-""-.        Abandon hope all ye who enter here                   .-""-.
-- / _  _ \                                                           / _  _ \
-- |(_)(_)|         i.e. I wouldn't build against any of this yet     |(_)(_)|
-- (_ /\ _)                                                           (_ /\ _)
--  L====J                                                             L====J
--  '-..-'                                                             '-..-'
--
------------------------------------------------------------------------------

-- TODO: check the following text for outright fabrications/lies

-- | Blaaargh! is a simple filesystem-based content management system
-- for static or semi-static publishing. You can^H^H^H will be able to
-- run it standalone or integrate it as a component in a larger
-- happstack application.
--
-- Features:
--
-- * takes a collection of markdown-formatted content (pages/posts/articles)
--   in a "content area" on the filesystem and formats it into HTML using
--   user-supplied cascading templates
--
-- * requesting a directory will invoke a custom template called
--   "index.st". We expose various views of the list of posts to the
--   templates, i.e.:
--
--   $recentPosts:{ post | <li><a href="$post.url$">$post.title$</a></li>}$
--   $chronologicalPosts:...$
--
-- * can spit out atom feeds for directories in the content area (if
--   they have index templates)
--
-- * can read configurable parameters (title, directories to be
--   excluded from the feed, etc) from an ini-style file
--
--
-- Missing features:
--
-- * support for any content that wouldn't make sense as an entry in an atom
-- feed
--
-- * a web-accessible "administrative area"
--
-- * a comment system (currently just outsourcing this to Disqus)
--
--
-- A post on the disk is stored in the content area:
-- $(blaaargh_dir)/content/. Let's say you have some files in here:
--
-- config
-- content/posts/foo.md
-- content/posts/bar.md
-- content/posts/bar/photo.jpg
-- content/static/spatula.md
-- templates/static/post.st     -- called when a post is requested
-- templates/static/home.st     -- called when /static is requested
-- templates/static/spatula.st  -- you can have a special template for
--                                 one page
--
--
-- Think of it like this -- you request content, either a single post,
-- a directory, or an atom feed (hardcoded as feed.xml). Blaaargh
-- searches for the closest matching template. For a content file
-- /foo/bar/baz.md, we would search templates/ for:
--
-- * foo/bar/baz.st
-- * foo/bar/post.st
-- * foo/post.st
-- * post.st
-- * 404.st
--
-- For a directory /foo/bar/quux, we search templates/ for
-- "foo/bar/quux/index.st".
--
-- (i.e. we don't walk up the directory structure)
--
-- And if content/foo/bar/quux/index.md exists, we read it into the template
-- as content text.
--
--
-- The documents get indexed into a map by id. The '.md' suffices get
-- stripped off and those files are treated as posts, and other files
-- are served as static files:
--
--     posts/foo
--     posts/bar
--     posts/bar/photo.jpg -- served as static
--     static/hamster
--
--
-- Nothing else fancy is going on yet --- blaaargh will not yet automatically
-- organize your posts by time (e.g. /posts/2009/05/26/post-slug), although
-- it's easy to do so by hand.
--
-- A couple of things blaaargh WILL give you:
--
-- * a request for a directory ending in feed.xml (e.g.  /posts/2009/feed.xml)
--   will return an RSS feed of the last N within (configurable), in reverse
--   chronological order
--
-- * requesting a directory (e.g. /posts/) will render a "post list" using a
-- custom stringtemplate. Template variables filled include:
--
-- ** lists
-- $alphabeticalPosts$
-- $recentPosts$
-- $chronologicalPosts$
-- $reverseChronologicalPosts$
--
-- a post is served to the template with the following attributes:
--     $url$
--     $title$
--     $date$
--     $body$
--     $summary$
--     etc
--
-- When serving a single page, the post data is stored in the template
-- variable "post", i.e. writing $post.url$ will substitute as
-- expected.


-- to make things super-easy on us, we'll define our internal post
-- format to be the same as our Atom feed.
newtype Post = Post { unPost :: Atom.Entry }
  deriving (Show)


getPostTime :: Post -> ZonedTime
getPostTime (Post p) = parseAtomTime $ fromMaybe upd pub
  where
    pub = Atom.entryPublished p
    upd = Atom.entryUpdated p


instance ToSElem Atom.EntryContent where
    toSElem (Atom.TextContent s)  = toSElem s
    toSElem (Atom.HTMLContent s)  = toSElem s
    toSElem _                     = toSElem (""::String)


instance ToSElem Atom.TextContent where
    toSElem (Atom.TextString s) = toSElem s
    toSElem (Atom.HTMLString s) = toSElem s
    toSElem _ = toSElem (""::String)


instance ToSElem Atom.Person where
    toSElem (Atom.Person name _ email _) = toSElem $ name ++ em
      where
        em = maybe "" (\e -> " <" ++ e ++ ">") email


instance ToSElem Post where
    toSElem post@(Post p) = SM $ Map.fromList attrs
      where
        url     = Atom.entryId p
        body    = fromMaybe (Atom.TextContent "") $ Atom.entryContent p
        summary = fromMaybe (Atom.HTMLString "") $ Atom.entrySummary p

        attrs   = [ ("id",      toSElem url)
                  , ("date",    toSElem $ friendlyTime $ getPostTime post)
                  , ("url",     toSElem url)
                  , ("title",   toSElem $ Atom.entryTitle p)
                  , ("content", toSElem body)
                  , ("summary", toSElem summary)
                  , ("authors", toSElemList $ Atom.entryAuthors p) ]



instance ToMessage Atom.Feed where
    toContentType _ = "application/atom+xml"
    toMessage     f = L.pack $ XML.showElement $ Atom.xmlFeed f



type ContentMap = Map ByteString ContentItem

data ContentItem =
       ContentPost Post                        -- ^ a post
     | ContentDirectory ByteString ContentMap  -- ^ a path prefix + content
                                               --   mapping
     | ContentStatic FilePath                  -- ^ a static file
  deriving (Show)


data BlaaarghState = BlaaarghState
    { blaaarghPath      :: FilePath      -- ^ path on disk
    , blaaarghSiteURL   :: String        -- ^ site URL, minus slash
                                         --   (e.g. http://foo.com)
    , blaaarghBaseURL   :: String        -- ^ base URL of content section,
                                         --   e.g. "/posts"
    , blaaarghPostMap   :: ContentMap    -- ^ content
    , blaaarghTemplates :: TemplateDirs  -- ^ templates
    , blaaarghFeedInfo  :: Atom.Feed     -- ^ feed info

    , blaaarghFeedExcludes :: ExcludeList -- ^ these URLs won't appear in
                                          -- feeds or in post listings

    , blaaarghExtraTmpl :: Template -> Template
                                         -- ^ extra template variables get
                                         --   inserted here
    }


type BlaaarghMonad   = StateT BlaaarghState IO
type BlaaarghHandler = ServerPartT BlaaarghMonad Response


liftB :: ServerPartT IO a -> ServerPartT BlaaarghMonad a
liftB = mapServerPartT liftIO


runBlaaarghHandler :: BlaaarghState
                   -> ServerPartT BlaaarghMonad a
                   -> ServerPartT IO a
runBlaaarghHandler s = mapServerPartT $ \m -> do
                         (a,_) <- runStateT m s
                         return a



addExtraTemplateArguments :: ToSElem a =>
                             [(String,a)]
                          -> BlaaarghMonad ()
addExtraTemplateArguments args = do
    modify $ \t ->
        t { blaaarghExtraTmpl = foldl f (blaaarghExtraTmpl t) args }

  where
    f :: ToSElem a => (Template -> Template) -> (String, a) -> (Template -> Template)
    f xtmpl (k,v) = (setAttribute k v) . xtmpl
