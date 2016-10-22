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
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M

data CollectionOrderingDirection
  = Ascending
  | Descending
  | Ambiguous
  deriving(Show,Enum,Ord,Eq)

data CoreConfigCollection = CoreConfigCollection
  { collectionLayout :: Maybe String
  , collectionOrdering :: Maybe (CollectionOrderingDirection, String)
  } deriving (Show)

data CoreConfig = CoreConfig
  { coreLayout :: Maybe String
  , coreOutput :: Maybe String
  , coreCollection :: CoreConfigCollection
  , coreCollections :: M.Map String (Maybe CoreConfig)
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
    let ordering = (\a b -> (a,b)) <$> o <*> ok
    return (CoreConfigCollection l ordering)
  parseJSON invalid = AT.typeMismatch "collection" invalid

instance Y.ToJSON CoreConfigCollection where
  toJSON cc = Y.object (catMaybes
    [ (\x -> "layout" .= x) <$> collectionLayout cc
    , (\(o,_) -> "ordering" .= o) <$> collectionOrdering cc
    , (\(_,ok) -> "order key" .= ok) <$> collectionOrdering cc

    ])

instance Y.FromJSON CoreConfig where
  parseJSON (Y.Object v) = CoreConfig
    <$> v .:? "layout"
    <*> v .:? "output"
    <*> v .:? "collection" .!= (CoreConfigCollection Nothing Nothing)
    <*> v .:? "collections" .!= M.empty
    <*> v .:? "title"
    <*> v .:? "contents"
    <*> pure v
    <*> pure ""
  parseJSON invalid = AT.typeMismatch "config" invalid

instance Y.ToJSON CoreConfig where
  toJSON c = Y.Object $ HM.union (HM.fromList (catMaybes
    [ ("layout" .=) <$> coreLayout c
    , ("output" .=) <$> coreOutput c
    , case coreCollection c of
        CoreConfigCollection Nothing Nothing -> Nothing
        cc -> Just ("collection" .= cc)
    , if M.null (coreCollections c)
      then Nothing
      else Just ("collections" .= coreCollections c)
    , ("title" .=) <$> coreTitle c
    , ("contents" .=) <$> coreContents c
    , Just ("url" .= coreURL c)
    ])) (coreExtras c)

emptyConfig = CoreConfig
  { coreLayout = Nothing
  , coreOutput = Nothing
  , coreCollection = (CoreConfigCollection Nothing Nothing)
  , coreCollections = M.empty
  , coreTitle = Nothing
  , coreContents = Nothing
  , coreExtras = HM.empty
  , coreURL = "."
  }

overlay :: CoreConfig -> String -> CoreConfig -> CoreConfig
overlay c k nc =
  let Just out = coreOutput c
      nl = case coreLayout nc of
        Nothing -> collectionLayout (coreCollection c)
        Just l -> Just l
  in nc
    { coreLayout = nl
    , coreExtras = HM.union (coreExtras nc) (coreExtras c)
    , coreOutput = Just (out ++ "/" ++ k)
    }



subload :: FilePath -> IO CoreConfig
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

loadText :: String -> FilePath -> IO (String, Maybe CoreConfig)
loadText parent path = do
  let fpath = fileName path
  text <- B.readFile path
  if B.isPrefixOf "---" text
    then do
      let (yaml,text1) = B.breakSubstring "\n---" (B.drop 3 text)
          text2 = B.drop 4 text1
          config = case Y.decodeEither yaml of
            Left err -> error ("YAML could not be parsed in " ++ path ++ " error: " ++ err)
            Right conf -> conf
              { coreContents= Just (Y.String (T.decodeUtf8 text2))
              , coreURL=parent ++ "/" ++ fpath ++ "/index.html"
              }
      return (fpath, Just config)
    else return (fpath, Just $ emptyConfig
      { coreContents = Just (Y.String (T.decodeUtf8 text))
      , coreURL = parent ++ "/" ++ fpath ++ "/index.html"
      })

load :: FilePath -> String -> CoreConfig -> IO CoreConfig
load root parent config = do
  let subcols = M.keys (coreCollections config)
  cols <- forM subcols $ \col -> do
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
        scl <- load colroot (parent) sc
        return [(col, Just $ scl {coreURL = parent ++ "/" ++ col ++ "/index.html"})]
  let res = config
        { coreCollections = M.fromList (concat cols)
        , coreURL = "index.html"
        }
  return res

renderMustache :: FilePath -> String -> String -> Y.Value -> IO ()
renderMustache root layout output config = do
  let ldir = root ++ "/layout"
  res <- MS.automaticCompile [ldir] layout
  template <- case res of
    Left e -> fail (show e)
    Right t -> return t
  let rendered = MS.substitute template config
      utf8render = T.encodeUtf8 rendered
  B.writeFile output utf8render

renderCore :: FilePath -> CoreConfig -> IO ()
renderCore root c = do
  let Just output = coreOutput c
      layout = case coreLayout c of
        Nothing -> Nothing
        Just l -> case stripPrefix ("/layout/") l of
          Nothing -> Just l
          Just x -> Just x
      subcols = M.toList $ coreCollections c
  case layout of
    Nothing -> return ()
    Just l -> do
      let Y.Object cval1 = Y.toJSON c
          arrCol = Y.toJSON (M.elems (coreCollections c))
          cval2 = HM.insert "elems" arrCol cval1
      renderMustache root l (output ++ "/index.html") (Y.Object cval2)
  forM_ subcols $ \(k, sc) -> do
    D.createDirectory (output ++ "/" ++ k)
    case sc of
      Nothing -> return ()
      Just scc -> do
        let osc = overlay c k scc
        renderCore root osc

render :: FilePath -> IO ()
render root = do
  config <- subload (root ++ "/site.yaml")
  lconfig <- load root "." config
  -- We expect the root to have a layout which will be the
  -- index.html we are looking for, as well as an output.
  let output = case coreOutput lconfig of
        Nothing -> error "Output needed on root configuration"
        Just "" -> error "Output needed on root configuration"
        Just x -> root ++ "/" ++ x
  case coreLayout lconfig of
    Nothing -> error "Layout needed on root configuration"
    Just "" -> error "Layout needed on root configuration"
    Just _ -> return ()
  -- Now that we have an output, let's make sure the path exists
  -- and that we clean it out. Hopefully there's nothing else in there
  -- it would be a shame if it were "/"
  direxists <- D.doesDirectoryExist output
  -- Clean up
  when direxists (D.removeDirectoryRecursive output)
  -- Make directory
  D.createDirectory output

  renderCore root (lconfig
    { coreOutput = Just output
    })

  B.writeFile (root ++ "/_compiled.yaml") (Y.encode lconfig)
