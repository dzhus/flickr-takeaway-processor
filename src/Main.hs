{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE TypeFamilies    #-}

module Main where

import           ClassyPrelude                 as P
                                         hiding ( FilePath
                                                , id
                                                )

import           Control.Concurrent.Async.Extra
import           Control.Monad.Logger
import           Data.Aeson                    as A
                                         hiding ( Options )

import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Text.Read
import           Turtle                  hiding ( f
                                                , o
                                                )

import           Filesystem.Path                ( extensions
                                                , replaceExtensions
                                                )

import qualified Control.Foldl                 as Fold
import qualified Data.Text                     as T

data Options = Options
  { metaDir    :: FilePath
  , mediaDir   :: FilePath
  , maxThreads :: Int
  }

data Geo = Geo
  { latitude  :: Float
  , longitude :: Float
  }
  deriving Show

instance FromJSON Geo where
  parseJSON =
    withObject "Geo" $
    \o -> Geo <$> (conv =<< (o .: "latitude")) <*> (conv =<< (o .: "longitude"))
    where
      conv txt = case rational txt of
                   Right (num, _) -> return $ num / 1e6
                   _              -> fail "Could not read Geo components"

newtype Tag = Tag { unTag :: Text }
  deriving Show

instance FromJSON Tag where
  parseJSON = withObject "Tag" $ \o -> Tag <$> (o .: "tag")

newtype Album = Album Text
  deriving Show

instance FromJSON Album where
  parseJSON = withObject "Album" $ \o -> Album <$> (o .: "title")

data Privacy = Public | FriendsAndFamily | Private
  deriving (Eq, Show)

instance FromJSON Privacy where
  parseJSON =
    withText "Privacy" $
    \case
      "friend & family" -> return FriendsAndFamily
      "private"         -> return Private
      "public"          -> return Public
      _                 -> fail "Unexpected privacy value"

-- | Flickr sidecar JSON structure.
data PhotoMeta = PhotoMeta
  { id          :: Text
  , date_taken  :: LocalTime
  , description :: Text
  , name        :: Text
  , geo         :: Maybe Geo
  , albums      :: [Album]
  , tags        :: [Tag]
  , privacy     :: Privacy
  }
  deriving (Generic, Show)

instance FromJSON PhotoMeta

showF :: FilePath -> Text
showF = pack . encodeString

foldAsList :: MonadIO m => Shell a -> m [a]
foldAsList act = Turtle.fold act Fold.list

forConcurrentlyN
  :: (MonadUnliftIO m, Traversable t) => Int -> t a -> (a -> m b) -> m (t b)
forConcurrentlyN n inputs act = mapConcurrentlyBounded n act inputs

optParser :: Parser Options
optParser =
  Options
    <$> argPath "meta"  "Directory containing photo_*.json files"
    <*> argPath "media" "Directory with media files"
    <*> (optInt "threads" 't' "Maximum number of threads for I/O-bound operations" <|> pure 10)

parseSidecar :: MonadIO m => FilePath -> m (Maybe PhotoMeta)
parseSidecar jf = do
  res <- liftIO $ P.readFile $ encodeString jf
  return $ decode $ fromStrict res

-- | Return path to the photo file the sidecar corresponds to.
findFile
  :: [FilePath]
  -- ^ List of files to look for the photo in.
  -> PhotoMeta
  -> Maybe FilePath
findFile mediaFiles PhotoMeta {..} =
  P.find (((id <> "_o") `isInfixOf`) . showF) mediaFiles <|>
  P.find           ((id `isInfixOf`) . showF) mediaFiles

-- | Produce an privacy-derived path for the file.
privateFilename :: PhotoMeta -> FilePath -> FilePath
privateFilename PhotoMeta {..} path =
  case (privacy, "private" `elem` exts) of
    (Private, False) -> replaceExtensions path ("private":exts)
    _                -> path
    where
      exts = extensions path

-- | Produce an album-derived path for the file.
albumFilename :: PhotoMeta -> FilePath -> FilePath
albumFilename PhotoMeta {..} path =
  case headMay albums of
    Just (Album a) ->
      if fromText a `elem` map dirname (splitDirectories path)
      then path
      else directory path <> fromText a <> filename path
    Nothing -> path

-- | If the file name has a leading dash, produce a new one.
fixedFilename :: FilePath -> FilePath
fixedFilename path = case unpack $ showF (filename path) of
  '-' : fileSuffix -> directory path <> fromText ("photo" <> pack fileSuffix)
  _                -> path

-- | Move/rename the photo if needed and return new path.
renameFile
  :: MonadLoggerIO m
  => [FilePath]
  -- ^ List of files to look for the photo in.
  -> PhotoMeta
  -> m (Maybe FilePath)
renameFile mediaFiles pm@PhotoMeta {..} = case findFile mediaFiles pm of
  Nothing  -> return Nothing
  Just path ->
    let
      targetPath = privateFilename pm $ albumFilename pm $ fixedFilename path
    in
      if targetPath /= path
      then do
        logDebugN $ "Moving " <> showF path <> " to " <> showF targetPath
        mktree (directory targetPath)
        mv path targetPath
        return $ Just targetPath
      else
        return Nothing

makeExiftoolTags :: PhotoMeta -> [(Text, Text)]
makeExiftoolTags PhotoMeta {..} =
  [ ("ImageDescription"         , T.strip description)
  , ("xmp-dc:Description"       , T.strip description)
  , ("Headline"                 , T.strip name)
  , ("xmp-dc:Title"             , T.strip name)
  , ("DateTimeOriginal"         , timestamp $ Just "%H:%M:%S")
  , ("xmp-exif:DateTimeOriginal", timestamp $ Just "%H:%M:%S")
  , ("xmp-dc:Date"              , timestamp Nothing)
  , ("CodedCharacterSet"        , "UTF-8")
  ] <>
  map (("keywords",) . unTag) tags <>
  maybe [] geoTags geo
  where
    timestamp fmt = pack $
      formatTime defaultTimeLocale (iso8601DateFormat fmt)
      date_taken
    -- "ExifTool is very flexible about the input format when writing
    -- lat/long coordinates, and will accept .. floating point numbers"
    geoTags Geo {..} =
      map (\(k, v) -> (k, tshow v))
      -- "ExifTool will also accept a number when writing
      -- GPSLatitudeRef, positive for north latitudes or negative for
      -- south"
      [ ("GPSLatitudeRef"       , signum latitude)
      , ("GPSLatitude"          , latitude)
      , ("xmp-exif:GPSLatitude" , latitude)
      , ("GPSLongitudeRef"      , signum longitude)
      , ("GPSLongitude"         , longitude)
      , ("xmp-exif:GPSLongitude", longitude)
      ]

-- | Product a list of exiftool command arguments.
--
-- Apparently we don't need to bother with quotes if we don't use any
-- shell ('procStrictWithErr').
formatExiftoolTags :: [(Text, Text)] -> [Text]
formatExiftoolTags = map (\(k, v) -> "-" <> k <> "=" <> v)

exiftoolOptions :: [Text]
exiftoolOptions =
  [ "-charset"
  , "exif=UTF-8"
  , "-charset"
  , "iptc=UTF-8"
  , "-overwrite_original_in_place"
  ]

main :: IO ()
main = runStdoutLoggingT $ do
  Options {..} <- options "Process Flickr takeaway files" optParser
  jsons        <- foldAsList
    $ Turtle.find (contains "photo_" *> suffix ".json") metaDir
  sidecars <- catMaybes <$> forConcurrentlyN maxThreads jsons parseSidecar
  logInfoN $ format (d % "/" % d % " sidecar .json files parsed")
                    (length sidecars)
                    (length jsons)

  -- Rename files first
  foldAsList (Turtle.lsdepth 1 2 mediaDir) >>= \originalMediaFiles -> do
    moveResults <- fmap (length . filter isJust) $
                   forConcurrentlyN maxThreads sidecars $
                   renameFile originalMediaFiles
    when (moveResults > 0) $
      logInfoN $ format ("Renamed " % d % " files") moveResults

  -- Include files which may have been moved to album subdirectories
  -- already
  mediaFiles <- foldAsList $ Turtle.lsdepth 1 2 mediaDir
  tasks      <- forConcurrently sidecars $ \sc ->
    return $ case findFile mediaFiles sc of
      Nothing -> Left sc
      Just f  -> Right (f, makeExiftoolTags sc)

  unless (null $ lefts tasks) $
    logErrorN $ format
    ("Media files not found for " % d % " sidecars: " % s)
    (length $ lefts tasks)
    (tshow $ map id $ lefts tasks)

  logInfoN $ format ("Writing tags for " % d % " files") (length $ rights tasks)

  results <- fmap concat $ forConcurrentlyN maxThreads (rights tasks) $
    \(f, tags) ->
      -- For some reason exiftool can't process JSON when SourceFile
      -- field refers to a file with a space in the path, so we'll do
      -- all files one by one, passing all tags and target file via
      -- command arguments.
      foldAsList $ do
      (ex, _, errOutput) <- procStrictWithErr
        "exiftool"
        (formatExiftoolTags tags <> exiftoolOptions <> [showF f])
        empty
      return $ case ex of
        ExitSuccess   -> Right (f, T.strip errOutput)
        ExitFailure _ -> Left (f, T.strip errOutput)

  forM_ (rights results) $ \(f, errOutput) ->
    when ("warning" `isInfixOf` T.toLower errOutput) $
    logWarnN $ format ("Warnings for " % fp % ": " % s) f errOutput

  let errors = lefts results

  forM_ errors $ \(f, errOutput) ->
    logErrorN $ format ("Errors for " % fp % ": " % s) f errOutput

  logInfoN $ format
    ("Exiftool sucessfully ran for " % d % " files")
    (length $ rights results)

  unless (null errors) $
    logErrorN $ format (d % " errors occured") (length errors)
