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
import           Turtle                  hiding ( o )

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

findFile
  :: [FilePath]
  -- ^ List of files to look for the photo in.
  -> PhotoMeta
  -> Maybe FilePath
findFile mediaDir PhotoMeta {..} =
  P.find (((id <> "_o") `isInfixOf`) . showF) mediaDir

exiv2Commands :: PhotoMeta -> [Text]
exiv2Commands PhotoMeta {..} =
  [ "-M"
  , "set Iptc.Application2.Headline String " <> name
  -- , "-M", "set Iptc.Application2.Caption String " <> description
  , "-M"
  , "set Exif.Photo.DateTimeOriginal Ascii " <> timestamp
  ]
 where
  timestamp =
    pack
      $ formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S")
      $ unwrap date_taken

embedSidecar
  :: (MonadUnliftIO m, MonadLogger m) => PhotoMeta -> FilePath -> m (Maybe Int)
embedSidecar pm photoPath = do
  $logDebug $ "embedSidecar: " <> showF photoPath
  try (sh $ inprocWithErr "exiv2" (exiv2Commands pm <> [showF photoPath]) empty)
    >>= \case
          Right _ -> return Nothing
          Left (ExitFailure n) -> return $ Just n
          Left ExitSuccess -> error "embedSidecar: exception on exiv2 success"

data ProcessingError = FileNotFound
                     | Exiv2Failure Int
                     deriving Show

main :: IO ()
main = runStdoutLoggingT $ do
  Options {..} <- options "Process Flickr takeaway files" optParser
  sidecars     <- foldAsList $ do
    pushd metaDir
    Turtle.find (contains "photo_" *> suffix ".json") =<< pwd
  $logInfo $ format (d % " sidecar .json files found") (length sidecars)
  jsons      <- liftIO $ forConcurrentlyN 10 sidecars parseSidecar
  mediaFiles <- foldAsList $ Turtle.ls mediaDir
  res        <- forConcurrentlyN 5 (catMaybes jsons) $ \sc -> (sc, ) <$> do
    case findFile mediaFiles sc of
      Nothing -> return $ Just FileNotFound
      Just f  -> embedSidecar sc f >>= \case
        Just err -> return $ Just $ Exiv2Failure err
        Nothing  -> return Nothing
  print $ filter (isJust . snd) res
