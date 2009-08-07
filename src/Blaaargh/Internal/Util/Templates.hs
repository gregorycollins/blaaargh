{-# LANGUAGE OverloadedStrings #-}

module Blaaargh.Internal.Util.Templates
  ( TemplateDirectory
  , Template
  , TemplateGroup
  , readTemplateDir
  , lookupDirgroup
  )
where

------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Prelude hiding (catch)
import           System.Directory
import           System.FilePath
import           Text.StringTemplate

------------------------------------------------------------------------------
import           Blaaargh.Internal.Exception
import           Blaaargh.Internal.Util.ExcludeList

------------------------------------------------------------------------------

{-|

'TemplateDirectory' is a directory structure of 'StringTemplate's. 'Template's
are indexed by path from a root path \".\", e.g. \"./dir/foo\", and templates
can invoke other templates (from the same directory or a parent directory) by
name.

-}

data TemplateDirectory =
    TemplateDirectory TemplateGroup (Map ByteString TemplateDirectory)


instance Show TemplateDirectory where
    show x = help 0 x
      where
        help n (TemplateDirectory _ s) =
            "{\n" ++ concatMap (sone n) assocs
                  ++ "\n"
                  ++ (replicate n '\t')
                  ++ "}\n"
          where
            assocs = Map.assocs s


        sone n (k,v) = (replicate n '\t') ++
                         (B.unpack k) ++ " => " ++
                         (help (n+1) v)


-- | TemplateGroup is a type alias for a StringTemplate over ByteStrings.
type Template      = StringTemplate B.ByteString


-- | TemplateGroup is a type alias for a STGroup over ByteStrings.
type TemplateGroup = STGroup B.ByteString


------------------------------------------------------------------------------


-- | Given a directory on the filesystem, crawl it for ".st" files and
--   produce a TemplateDirectory.
readTemplateDir :: FilePath -> IO TemplateDirectory
readTemplateDir d = do
    mp <- help d
    return $ fixup mp

  where
    help path = do
        isDir <- doesDirectoryExist path
    
        when (not isDir)
             (throwIO $ BlaaarghException
                      $ "template directory '" ++ path ++ "' does not exist")
    
        grp <- directoryGroup path
    
        files <- getDirectoryContents path >>=
                   return .
                   filter (\x -> not ("." `isPrefixOf` x))
    
        dirs  <- filterM (\x -> doesDirectoryExist $ path </> x) files

        subDirs <- mapM (\f -> do
                           t <- help (path </> f) 
                           return (B.pack f,t))
                        dirs

        return $ TemplateDirectory grp $ Map.fromList subDirs


    addGroup grp (TemplateDirectory g sub) =
        TemplateDirectory (addSuperGroup g grp) sub


    fixup (TemplateDirectory grp sub) =
        TemplateDirectory grp newsub
      where
        sub'   = fmap (addGroup grp) sub
        newsub = fmap fixup sub'
        

lookupDirgroup :: FilePath -> TemplateDirectory -> Maybe TemplateGroup
lookupDirgroup path t = help pl t
  where
    stripDot []      = []
    stripDot (".":b) = b
    stripDot l       = l

    pl = stripDot $ fromPath $ B.pack path

    help []    (TemplateDirectory grp _) = Just grp
    help (a:b) (TemplateDirectory _ sub) = do
      td <- Map.lookup a sub
      help b td
