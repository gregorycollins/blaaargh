{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Blaaargh.Internal.Types 
  ( module Blaaargh.Internal.Exception
  , Post(..)
  , getPostTime
  , ContentMap
  , ContentItem(..)
  , BlaaarghState(..)
  , BlaaarghMonad(..)
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
import           Blaaargh.Internal.Exception
import           Blaaargh.Internal.Time
import           Blaaargh.Internal.Util.ExcludeList
import           Blaaargh.Internal.Util.Templates



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


{-|

BlaaarghState is an opaque data type that holds Blaaargh internal state.

-}
data BlaaarghState = BlaaarghState
    { blaaarghPath      :: FilePath      -- ^ path on disk
    , blaaarghSiteURL   :: String        -- ^ site URL, minus slash
                                         --   (e.g. http://foo.com)
    , blaaarghBaseURL   :: String        -- ^ base URL of content section,
                                         --   e.g. "/posts"
    , blaaarghPostMap   :: ContentMap    -- ^ content
    , blaaarghTemplates :: TemplateDirectory  -- ^ templates
    , blaaarghFeedInfo  :: Atom.Feed     -- ^ feed info

    , blaaarghFeedExcludes :: ExcludeList -- ^ these URLs won't appear in
                                          -- feeds or in post listings

    , blaaarghExtraTmpl :: Template -> Template
                                         -- ^ extra template variables get
                                         --   inserted here
    }


{-|

A BlaaarghMonad is a state monad over IO.

-}
newtype BlaaarghMonad a =
    BlaaarghMonad { unBlaaarghMonad :: StateT BlaaarghState IO a }
  deriving (Monad, MonadIO, MonadState BlaaarghState)


{-|

The 'ServerPartT' happstack handler type is a monad transformer; here we define
'BlaaarghHandler' as a 'ServerPartT' around our 'BlaaarghMonad' type.

-}
type BlaaarghHandler = ServerPartT BlaaarghMonad Response


liftB :: ServerPartT IO a -> ServerPartT BlaaarghMonad a
liftB = mapServerPartT liftIO



{-|

Given Blaaargh state and a happstack 'ServerPartT' over a 'BlaaarghMonad',
'runBlaaarghHandler' produces a 'ServerPartT' over 'IO' that you can pass to
happstack.

-}
runBlaaarghHandler ::
    BlaaarghState                -- ^ blaaargh state, obtained from calling
                                 -- 'initBlaaargh'
 -> ServerPartT BlaaarghMonad a  -- ^ a blaaargh handler
 -> ServerPartT IO a
runBlaaarghHandler s = mapServerPartT $ \m -> do
                         (a,_) <- runStateT (unBlaaarghMonad m) s
                         return a


{-|

Sometimes you want to pass extra key-value mappings to be served in Blaaargh
templates. For example:

@
lift (addExtraTemplateArguments [(\"foo\", \"foovalue\")]) \>\> serveBlaaargh
@

will cause the value @$foo$@ to be expanded as \"foovalue\" within templates
served by Blaaargh.

-}
addExtraTemplateArguments :: ToSElem a =>
                             [(String,a)]
                          -> BlaaarghMonad ()
addExtraTemplateArguments args = do
    modify $ \t ->
        t { blaaarghExtraTmpl = foldl f (blaaarghExtraTmpl t) args }

  where
    f :: ToSElem a => (Template -> Template) -> (String, a) -> (Template -> Template)
    f xtmpl (k,v) = (setAttribute k v) . xtmpl
