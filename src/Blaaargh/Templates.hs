{-# LANGUAGE OverloadedStrings #-}

module Blaaargh.Templates
  ( findTemplateForPost
  , findTemplateForDirectory )
where


------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad.State
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Prelude hiding (catch)
import           Text.StringTemplate
------------------------------------------------------------------------------
import           Blaaargh.Types
import           Blaaargh.Util.Templates


------------------------------------------------------------------------------
findTemplateForPost :: [ByteString]   -- ^ path to the post, relative
                                      -- to the "content/" directory;
                                      -- if the file is in
                                      -- "content/foo/bar/baz.md" then
                                      -- this list will contain
                                      -- ["foo", "bar", "baz"]
                    -> BlaaarghMonad (Maybe (Template))
findTemplateForPost pathList = do
    xformTmpl <- liftM blaaarghExtraTmpl get
    assert (not $ null pathList) (return ())

    mbT <- findFirstMatchingTemplate templatesToSearch
    return $ xformTmpl `fmap` mbT

  where
    postName = last pathList

    -- if post is at "foo/bar/baz.md", then containingDirs contains
    -- [["foo","bar"], ["foo"], []]
    containingDirs    = tail . reverse . inits $ pathList

    -- search for a template specific to this post first, then walk up
    -- the directory structure looking for a template named "post"

    firstTmpl = (listToPath $ head containingDirs, postName)

    templatesToSearch = firstTmpl :
                          map (\d -> (listToPath d, "post")) containingDirs


------------------------------------------------------------------------------
findTemplateForDirectory :: [ByteString]
                         -> BlaaarghMonad (Maybe (Template))
findTemplateForDirectory pathList = do
    templates <- liftM blaaarghTemplates get
    xformTmpl <- liftM blaaarghExtraTmpl get
    assert (not $ null pathList) (return ())

    let mbT = lookupTmpl templates (listToPath pathList, "index")
    return $ xformTmpl `fmap` mbT


------------------------------------------------------------------------------
-- local functions follow
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | look up whether a particular template exists
lookupTmpl :: TemplateDirs          -- ^ templates
           -> (String, ByteString)  -- ^ (dir, template), where "dir"
                                    -- starts with "./"
           -> Maybe (StringTemplate ByteString)
lookupTmpl tmpls (d,t) =
    lookupDirgroup d tmpls >>= getStringTemplate (B.unpack t)


------------------------------------------------------------------------------
-- | Take a path list ["foo","bar","baz"] and turn it into "./foo/bar/baz"
listToPath :: [ByteString] -> String
listToPath l = B.unpack . B.concat $ intersperse "/" (".":l)


------------------------------------------------------------------------------
findFirstMatchingTemplate :: [(String,ByteString)]
                          -> BlaaarghMonad (Maybe (StringTemplate ByteString))
findFirstMatchingTemplate templatesToSearch = do
    templates <- liftM blaaarghTemplates get

    return . getFirst . mconcat $
      map (First . lookupTmpl templates) templatesToSearch




