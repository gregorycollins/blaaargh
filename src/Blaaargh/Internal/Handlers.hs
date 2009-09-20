{-# LANGUAGE OverloadedStrings #-}

module Blaaargh.Internal.Handlers ( serveBlaaargh ) where

import           Control.Monad.State
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.ByteString.Char8 (ByteString)
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Happstack.Server
import           Happstack.Server.HTTP.FileServe
import           Happstack.Server.Parts
import qualified Text.Atom.Feed as Atom
import           Text.Printf
import           Text.StringTemplate

------------------------------------------------------------------------------
import           Blaaargh.Internal.Post
import           Blaaargh.Internal.Templates
import           Blaaargh.Internal.Types
import qualified Blaaargh.Internal.Util.ExcludeList as EL


------------------------------------------------------------------------------
{-|

The top-level happstack handler. The 'BlaaarghHandler' is a 'ServerPartT' over
a state monad; you \"run\" this handler by feeding it a BlaaarghState using
'runBlaaarghHandler'. It handles requests on its base url (defined in the
@{blaaargh_dir}/config@ file) and serves up content from the content area.

-}
serveBlaaargh :: BlaaarghHandler
serveBlaaargh = do
    methodOnly GET
    compressedResponseFilter

    cm    <- lift get >>= return . blaaarghPostMap
    paths <- askRq >>= return . map B.pack . rqPaths

    serve [] paths cm `mappend` fourohfour

  where
    --------------------------------------------------------------------------
    serve :: [ByteString] -> [ByteString] -> ContentMap -> BlaaarghHandler
    serve soFar paths content = do
        case paths of
          []      -> serveIndex soFar content
          (a:[])  -> serveFile soFar a content
          (a:b)   -> serveDir soFar a b content


    --------------------------------------------------------------------------
    serveFile :: [ByteString] -> ByteString -> ContentMap -> BlaaarghHandler
    serveFile soFar a content = do
        if a == "feed.xml" then
            serveFeed soFar content
          else
            maybe mzero
                  (\f -> case f of
                           (ContentStatic fp)     -> serveStatic fp
                           (ContentPost post)     -> servePost (soFar ++ [a]) post
                           (ContentDirectory _ d) -> serveIndex (soFar ++ [a]) d)
                  (Map.lookup a content)


    --------------------------------------------------------------------------
    serveDir :: [ByteString]
             -> ByteString
             -> [ByteString]
             -> ContentMap
             -> BlaaarghHandler
    serveDir soFar d rest content = do
        let mbD = Map.lookup d content

        maybe mzero
              (\f -> case f of
                       (ContentDirectory _ mp) -> serve (soFar ++ [d]) rest mp
                       _                       -> mzero)
              mbD



------------------------------------------------------------------------------
fourohfour :: BlaaarghHandler
fourohfour = do
    state  <- lift get
    mbTmpl <- findFourOhFourTemplate
    tmpl   <- maybe mzero return mbTmpl

    let title = getTextContent . Atom.feedTitle . blaaarghFeedInfo $ state

    let tmpl'  = setAttribute "pageTitle" title tmpl

    return $ toResponse $ HtmlResponse $ render tmpl'


------------------------------------------------------------------------------
serveStatic :: FilePath -> BlaaarghHandler
serveStatic = localRq (\r -> r { rqPaths=[]}) . fileServeStrict []


newtype HtmlResponse = HtmlResponse ByteString

instance ToMessage HtmlResponse where
    toContentType _                = "text/html"
    toMessage     (HtmlResponse s) = L.fromChunks [s]


------------------------------------------------------------------------------
servePost :: [ByteString] -> Post -> BlaaarghHandler
servePost soFar post = do
    state  <- lift get
    mbTmpl <- lift $ findTemplateForPost soFar
    tmpl   <- maybe mzero return mbTmpl

    let title = concat
                  [ getTextContent . Atom.feedTitle . blaaarghFeedInfo $ state
                  , (let s = getTextContent . Atom.entryTitle . unPost $ post
                     in if null s then "" else ": " ++ s)
                  ]


    let tmpl'  = setAttribute "post" post $
                 setAttribute "pageTitle" title tmpl

    return $ toResponse $ HtmlResponse $ render tmpl'


------------------------------------------------------------------------------
getTextContent :: Atom.TextContent -> String
getTextContent (Atom.TextString s) = s
getTextContent (Atom.HTMLString s) = s
getTextContent _                   = undefined -- don't support that yet


------------------------------------------------------------------------------
getContentTitle :: ContentItem -> String
getContentTitle (ContentPost (Post p)) = getTextContent . Atom.entryTitle $ p
getContentTitle _                      = ""


------------------------------------------------------------------------------
serveIndex :: [ByteString] -> ContentMap -> BlaaarghHandler
serveIndex soFar content = do
    state  <- lift get
    mbTmpl <- lift $ findTemplateForDirectory soFar
    tmpl   <- maybe mzero return mbTmpl

    let excludes' =  blaaarghFeedExcludes state
    let excludes  =  foldl' (flip EL.descend) excludes' soFar

    let alpha     =  alphabeticalPosts excludes content
    let chron     =  chronologicalPosts excludes content
    let rchron    =  reverseChronologicalPosts excludes content
    let recent    =  take 5 rchron

    let postmap  = [ ("alphabeticalPosts"         , alpha)
                   , ("chronologicalPosts"        , chron)
                   , ("reverseChronologicalPosts" , rchron)
                   , ("recentPosts"               , recent) ]

    let tmpl'  = setManyAttrib postmap tmpl

    let mbPost = Map.lookup "index" content

    let baseURL  = B.pack $ blaaarghBaseURL state
    let fdPath     = B.concat $ intersperse "/" $ soFar ++ ["feed.xml"]
    let feedURL  = B.unpack $ B.concat [baseURL, "/", fdPath]


    let title = concat
                  [ getTextContent . Atom.feedTitle . blaaarghFeedInfo $ state
                  , maybe ""
                          (\x -> let s = getContentTitle x
                                 in if null s then "" else ": " ++ s)
                          mbPost
                  ]


    let tmpl'' = case mbPost of
                   (Just (ContentPost p)) -> setAttribute "post" p tmpl'
                   _                      -> tmpl'

    let autoDiscovery' = printf "<link rel=\"alternate\" \
                                 \type=\"application/atom+xml\" \
                                 \href=\"%s\">"
                                 feedURL :: String

    let autoDiscovery = if EL.matchList soFar excludes
                          then ""
                          else autoDiscovery'

    let tmpl''' = setAttribute "pageTitle" title $
                  setAttribute "extraHead" autoDiscovery tmpl''

    return $ toResponse $ HtmlResponse $ render tmpl'''


------------------------------------------------------------------------------
addSiteURL :: String -> Post -> Post
addSiteURL siteURL (Post p) =
    Post $ p {Atom.entryId = concat [siteURL, Atom.entryId p]}


------------------------------------------------------------------------------
serveFeed :: [ByteString] -> ContentMap -> BlaaarghHandler
serveFeed soFar content = do
    state <- lift get

    let excludes' =  blaaarghFeedExcludes state
    let excludes  =  foldl' (flip EL.descend) excludes' soFar

    let siteURL'  =  blaaarghSiteURL state
    let posts     =  map (addSiteURL siteURL') $ recentPosts excludes content 5
    hasTemplate   <- lift $ liftM isJust $ findTemplateForDirectory soFar

    if null posts || not hasTemplate
      then mzero
      else do
        let siteURL  = B.pack siteURL'
        let baseURL  = B.pack $ blaaarghBaseURL state
        let fdPath     = B.concat $ intersperse "/" $ soFar ++ ["feed.xml"]
        let feedURL  = B.unpack $ B.concat
                                $ [siteURL, baseURL, "/", fdPath]
        let baseFeed = blaaarghFeedInfo state

        let feed     = baseFeed {
                            Atom.feedId      = feedURL
                          , Atom.feedLinks   = [ Atom.nullLink feedURL ]
                          , Atom.feedEntries = map unPost posts
                          , Atom.feedUpdated = Atom.entryUpdated $ unPost (head posts)
                          }
        return $ toResponse feed
