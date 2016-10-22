{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
where

import Control.Monad
import Data.Maybe
import Data.List

import qualified Data.ByteString as B
import qualified Data.Yaml as Y
import qualified Data.Aeson.Types as AT
import Data.Aeson((.:),(.:?),(.!=),(.=))
import qualified Text.Mustache as MS
import qualified System.Directory as D
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified System.FilePath.Glob as G
import qualified Data.HashMap.Strict as M

data CollectionOrderingDirection
  = Ascending
  | Descending
  | Ambiguous
  deriving(Show,Enum,Ord,Eq)

data CoreConfigCollection = CoreConfigCollection
  { collectionLayout :: Maybe String
  , collectionOrdering :: Maybe (CollectionOrderingDirection, String)
  , collectionBreadcrumb :: Maybe String
  } deriving (Show)

data CoreConfig col = CoreConfig
  { coreLayout :: Maybe String
  , coreOutput :: Maybe String
  , coreBreadcrumb :: Maybe String
  , coreCollection :: CoreConfigCollection
  , coreCollections :: [col]
  , coreTitle :: Maybe String
  , coreContents :: Maybe Y.Value
  , coreExtras :: Y.Object
  , coreURL :: String
  } deriving(Show)

instance Y.FromJSON CollectionOrderingDirection where
  parseJSON (Y.String t) = case t of
    "asc" -> return Ascending
    "ascending" -> return Ascending
    "desc" -> return Descending
    "descending" -> return Descending
    "" -> return Ambiguous
    _ -> fail ("Unknown ordering direction: " ++ show t)
  parseJSON invalid = AT.typeMismatch "ordering" invalid

instance Y.ToJSON CollectionOrderingDirection where
  toJSON Ascending = "ascending"
  toJSON Descending = "descending"
  toJSON Ambiguous = ""

instance Y.FromJSON CoreConfigCollection where
  parseJSON (Y.Object v) = do
    l <- v .:? "layout"
    o <- v .:? "ordering"
    ok <- v .:? "order key"
    b <- v .:? "breadcrumb"
    let ordering = (\a b -> (a,b)) <$> o <*> ok
    return (CoreConfigCollection l ordering b)
  parseJSON invalid = AT.typeMismatch "collection" invalid

instance Y.ToJSON CoreConfigCollection where
  toJSON cc = Y.object (catMaybes
    [ (\x -> "layout" .= x) <$> collectionLayout cc
    , (\(o,_) -> "ordering" .= o) <$> collectionOrdering cc
    , (\(_,ok) -> "order key" .= ok) <$> collectionOrdering cc
    , (\x -> "breadcrumb" .= x) <$> collectionBreadcrumb cc
    ])

instance (Y.FromJSON col) => Y.FromJSON (CoreConfig col) where
  parseJSON (Y.Object v) = CoreConfig
    <$> v .:? "layout"
    <*> v .:? "output"
    <*> v .:? "breadcrumb"
    <*> v .:? "collection" .!= (CoreConfigCollection Nothing Nothing Nothing)
    <*> v .:? "collections" .!= []
    <*> v .:? "title"
    <*> v .:? "contents"
    <*> pure v
    <*> pure ""
  parseJSON invalid = AT.typeMismatch "config" invalid

instance (Y.ToJSON col) => Y.ToJSON (CoreConfig col) where
  toJSON c = Y.Object $ M.union (M.fromList (catMaybes
    [ ("layout" .=) <$> coreLayout c
    , ("output" .=) <$> coreOutput c
    , ("breadcrumb" .=) <$> coreBreadcrumb c
    , case coreCollection c of
        CoreConfigCollection Nothing Nothing Nothing -> Nothing
        cc -> Just ("collection" .= cc)
    , case coreCollections c of
        [] -> Nothing
        cs -> Just ("collections" .= cs)
    , ("title" .=) <$> coreTitle c
    , ("contents" .=) <$> coreContents c
    , Just ("url" .= coreURL c)
    ])) (coreExtras c)

newtype Config = Config (CoreConfig Config) deriving (Show)

instance Y.ToJSON Config where
  toJSON (Config c) = Y.toJSON c

emptyConfig = Config (CoreConfig
  { coreLayout = Nothing
  , coreOutput = Nothing
  , coreBreadcrumb = Nothing
  , coreCollection = (CoreConfigCollection Nothing Nothing Nothing)
  , coreCollections = []
  , coreTitle = Nothing
  , coreContents = Nothing
  , coreExtras = M.empty
  , coreURL = ""
  })

switchConfig :: CoreConfig a -> CoreConfig b
switchConfig config = (CoreConfig
  { coreLayout = coreLayout config
  , coreOutput = coreOutput config
  , coreBreadcrumb = coreBreadcrumb config
  , coreCollection = coreCollection config
  , coreCollections = []
  , coreTitle = coreTitle config
  , coreContents = coreContents config
  , coreExtras = coreExtras config
  , coreURL = coreURL config
  })

subload :: FilePath -> IO (CoreConfig String)
subload path = do
  exists <- D.doesFileExist path
  if exists
    then do
      bs <- B.readFile path
      case Y.decodeEither bs of
        Left err -> error ("YAML could not be parsed at " ++ path ++ " error: " ++ err)
        Right x -> return x
    else error ("File does not exist: " ++ path)

fileName xs = if elem '/' xs
  then let _:rest=dropWhile (/='/') xs in fileName rest
  else let name=takeWhile (/='.') xs in name

loadText :: String -> FilePath -> IO Config
loadText parent path = do
  text <- B.readFile path
  if B.isPrefixOf "---" text
    then do
      let (yaml,text1) = B.breakSubstring "\n---" (B.drop 3 text)
          text2 = B.drop 4 text1
          config = case Y.decodeEither yaml of
            Left err -> error ("YAML could not be parsed in " ++ path ++ " error: " ++ err)
            Right conf -> (switchConfig (conf :: CoreConfig [String]))
              { coreContents=Just (Y.String (T.decodeUtf8 text2))
              , coreURL=parent ++ "/" ++ fileName path
              }
      return (Config config)
    else return
      (let (Config c) = emptyConfig
      in Config (c
        { coreContents = Just (Y.String (T.decodeUtf8 text))
        , coreURL = parent ++ "/" ++ fileName path
        })
      )

load :: FilePath -> String -> CoreConfig String -> IO Config
load root parent config = do
  cols <- forM (coreCollections config) $ \col -> do
    if any (== '*') col
      then do
        let pat = G.compile col
        files <- D.listDirectory root
        let filtered = filter (G.match pat) files
            filteredP = map (\x -> root ++ "/" ++ x) filtered
        forM filteredP (loadText parent)
      else do
        let colroot = root ++ "/" ++ col
        sc <- subload (colroot ++ "/" ++ col ++ ".yaml")
        Config scl <- load colroot (parent ++ "/" ++ col) sc
        return [Config scl {coreURL = parent ++ "/" ++ col}]
  let res = (Config ((switchConfig config)
        { coreCollections = concat cols
        , coreURL = "/"
        }))
  return res

renderMustache :: Y.ToJSON a => FilePath -> String -> String -> a -> IO ()
renderMustache root layout output config = do
  let ldir = root ++ "/layout"
  res <- MS.automaticCompile [ldir] layout
  template <- case res of
    Left e -> fail (show e)
    Right t -> return t
  let rendered = MS.substitute template (Y.toJSON config)
      utf8render = T.encodeUtf8 rendered
  B.writeFile output utf8render

render :: FilePath -> IO ()
render root = do
  config <- subload (root ++ "/site.yaml")
  Config lconfig <- load root "" config
  -- We expect the root to have a layout which will be the
  -- index.html we are looking for, as well as an output.
  let output = case coreOutput lconfig of
        Nothing -> error "Output needed on root configuration"
        Just "" -> error "Output needed on root configuration"
        Just x -> root ++ "/" ++ x
      index1 = case coreLayout lconfig of
        Nothing -> error "Layout needed on root configuration"
        Just "" -> error "Layout needed on root configuration"
        Just x -> x
      index = case stripPrefix ("/layout/") index1 of
        Nothing -> index1
        Just x -> x
  -- Now that we have an output, let's make sure the path exists
  -- and that we clean it out. Hopefully there's nothing else in there
  -- it would be a shame if it were "/"
  direxists <- D.doesDirectoryExist output
  -- Clean up
  when direxists (D.removeDirectoryRecursive output)
  -- Make directory
  D.createDirectory output

  renderMustache root index (output ++ "/index.html") lconfig

  B.writeFile (root ++ "/_compiled.yaml") (Y.encode lconfig)
