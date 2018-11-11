{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

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
import           Data.Text.Read
import           Turtle                  hiding ( f
                                                , fp
                                                , o
                                                )

import qualified Control.Foldl                 as Fold
import qualified Data.Text                     as T

data Options = Options
  { metaDir  :: FilePath
  , mediaDir :: FilePath
  }

newtype FlickrUTCTime = FlickrUTCTime { unwrap :: UTCTime }
  deriving Show

instance FromJSON FlickrUTCTime where
  parseJSON =
    withText "FlickrUTCTime" $
    fmap FlickrUTCTime . parseJSON . String .
    -- Flickr sidecars contain no T separator and no timezone
    T.replace " " "T" . (<> "Z")

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

newtype Tag = Tag Text
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
  , date_taken  :: FlickrUTCTime
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

-- | If the file name has a leading dash, produce a new one.
fixedFilename :: FilePath -> Maybe FilePath
fixedFilename fp = case unpack $ showF fp of
  '-' : _ ->
    Just $ decodeString $ unpack $ "photo" <> T.dropWhile (== '-') (showF fp)
  _ -> Nothing

-- | Rename the photo if its filename has a leading dash.
renameMoveFile
  :: MonadLoggerIO m
  => [FilePath]
  -- ^ List of files to look for the photo in.
  -> PhotoMeta
  -> m Bool
renameMoveFile mediaFiles pm@PhotoMeta {..} = case findFile mediaFiles pm of
  Nothing  -> return False
  Just sth -> case fixedFilename $ filename sth of
    Just newFilename -> do
      let newPath = directory sth <> newFilename
      $logDebug $ "Moving " <> showF sth <> " to " <> showF newPath
      mv sth newPath
      return True
    _ -> return False

makeExiftoolTags :: PhotoMeta -> FilePath -> Map Text Text
makeExiftoolTags PhotoMeta {..} photoPath = mapFromList
  [ ("ImageDescription", T.strip description)
  , ("Headline"        , T.strip name)
  , ("DateTimeOriginal", timestamp)
  , ("SourceFile"      , showF photoPath)
  , ("CodedCharacterSet", "UTF-8")
  ]
 where
  timestamp =
    pack
      $ formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S")
      $ unwrap date_taken

data ProcessingError = FileNotFound
                     | Exiv2Failure Int
                     deriving Show

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
  sidecars <- catMaybes <$> forConcurrentlyN 10 jsons parseSidecar
  $logInfo $ format (d % "/" % d % " sidecar .json files parsed")
                    (length sidecars)
                    (length jsons)

  foldAsList (Turtle.ls mediaDir) >>= \originalMediaFiles -> do
    moveResults <- fmap (length . filter (== True)) $
                   forConcurrentlyN 10 sidecars $
                   renameMoveFile originalMediaFiles
    when (moveResults > 0) $
      $logInfo $ format ("Renamed " % d % " files") moveResults

  mediaFiles <- foldAsList $ Turtle.ls mediaDir
  res        <- forConcurrently sidecars $ \sc ->
    return $ case findFile mediaFiles sc of
      Nothing -> Left FileNotFound
      Just f  -> Right (f, makeExiftoolTags sc f)

  $logInfo $ format
    ("Media files not found for " % d % " sidecars")
    (length $ lefts res)

  $logInfo $ format ("Writing tags for " % d % " files") (length $ rights res)
  liftIO $ runManaged $ do
    (fp, tf) <- mktemp "." "exiftool.json"
    P.hPut tf $ toStrict $ encode $ map snd $ rights res
    sh $ inproc "exiftool"
      (["-j+=" <> showF fp] <> exiftoolOptions <> ["-@", "-"])
      (select $ mapMaybe (textToLine . showF . fst) $ rights res)
