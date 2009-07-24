{-# LANGUAGE OverloadedStrings #-}

module Blaaargh.Util.Templates
  ( TemplateDirs
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
import           Blaaargh.Exception
import           Blaaargh.Util.ExcludeList

------------------------------------------------------------------------------
data TemplateDirs =
     TemplateDirs TemplateGroup                  -- ^ top-level template group
                  (Map ByteString TemplateDirs)  -- ^ template group
                                                 -- for subdirs


instance Show TemplateDirs where
    show x = help 0 x
      where
        help n (TemplateDirs _ s) =
            "{\n" ++ concatMap (sone n) assocs
                  ++ "\n"
                  ++ (replicate n '\t')
                  ++ "}\n"
          where
            assocs = Map.assocs s


        sone n (k,v) = (replicate n '\t') ++
                         (B.unpack k) ++ " => " ++
                         (help (n+1) v)



type TemplateGroup = STGroup B.ByteString
type Template      = StringTemplate B.ByteString


------------------------------------------------------------------------------
readTemplateDir :: FilePath -> IO TemplateDirs
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

        return $ TemplateDirs grp $ Map.fromList subDirs


    addGroup grp (TemplateDirs g sub) =
        TemplateDirs (addSuperGroup g grp) sub


    fixup (TemplateDirs grp sub) =
        TemplateDirs grp newsub
      where
        sub'   = fmap (addGroup grp) sub
        newsub = fmap fixup sub'
        

lookupDirgroup :: FilePath -> TemplateDirs -> Maybe TemplateGroup
lookupDirgroup path t = help pl t
  where
    stripDot []      = []
    stripDot (".":b) = b
    stripDot l       = l

    pl = stripDot $ fromPath $ B.pack path

    help []    (TemplateDirs grp _) = Just grp
    help (a:b) (TemplateDirs _ sub) = do
      td <- Map.lookup a sub
      help b td
