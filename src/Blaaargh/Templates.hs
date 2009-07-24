{-# LANGUAGE OverloadedStrings #-}

module Blaaargh.Templates
  ( findFourOhFourTemplate
  , findTemplateForPost
  , findTemplateForDirectory )
where


------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad.State
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.List
import           Data.Monoid
import           Happstack.Server
import           Prelude hiding (catch)
import           Text.StringTemplate
------------------------------------------------------------------------------
import           Blaaargh.Types
import           Blaaargh.Util.Templates


------------------------------------------------------------------------------
findFourOhFourTemplate :: ServerPartT BlaaarghMonad (Maybe Template)
findFourOhFourTemplate = do
    pathList  <- askRq >>= return . map B.pack . rqPaths

    lift $ cascadingTemplateFind pathList "404"


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
    templates <- liftM blaaarghTemplates get
    assert (not $ null pathList) (return ())

    let ft  = First $ lookupTmpl templates firstTmpl
    st     <- cascadingTemplateFind pathList "post" >>= return . First
    let mbT = getFirst (ft `mappend` st)

    return $ xformTmpl `fmap` mbT

  where
    postName = last pathList

    -- search for a template specific to this post first, then walk up
    -- the directory structure looking for a template named "post"
    firstTmpl = (listToPath $ init pathList, postName)


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


------------------------------------------------------------------------------
cascadingTemplateFind :: [ByteString]
                      -> ByteString
                      -> BlaaarghMonad (Maybe (StringTemplate ByteString))
cascadingTemplateFind directories templateName = do
    assert (not $ null directories) (return ())

    findFirstMatchingTemplate templatesToSearch

  where
    -- if requested "foo/bar/baz", then containingDirs contains
    -- [["foo","bar"], ["foo"], []]
    containingDirs    = tail . reverse . inits $ directories

    templatesToSearch = map (\d -> (listToPath d, templateName))
                            containingDirs
