{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           ClassyPrelude           hiding ( FilePath, id )
import           Control.Concurrent.Async.Pool as Pool
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

newtype SpacedUTCTime = SpacedUTCTime { unwrap :: UTCTime }
  deriving (Show)

instance FromJSON SpacedUTCTime where
  parseJSON =
    withText "SpacedUTCTime" $
    fmap SpacedUTCTime . parseJSON . String .
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
  , date_taken  :: SpacedUTCTime
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

mapConcurrentlyN :: Traversable t => Int -> (a -> IO b) -> t a -> IO (t b)
mapConcurrentlyN n act inputs =
  withTaskGroup n $ \tg -> Pool.mapConcurrently tg act inputs

forConcurrentlyN :: Traversable t => Int -> t a -> (a -> IO b) -> IO (t b)
forConcurrentlyN n inputs act = mapConcurrentlyN n act inputs

optParser :: Parser Options
optParser =
  Options
    <$> argPath "meta"  "Directory containing photo_*.json files"
    <*> argPath "media" "Directory with media files"

parseSidecar :: MonadIO m => FilePath -> m (Maybe PhotoMeta)
parseSidecar jf = do
  res <- liftIO $ ClassyPrelude.readFile $ encodeString jf
  return $ decode $ fromStrict res

findFile :: MonadIO m
         => FilePath
         -- ^ Directory to look for file in.
         -> PhotoMeta
         -> m (Maybe FilePath)
findFile mediaDir PhotoMeta{..} =
  fmap headMay $
  foldAsList $ Turtle.find (contains $ text $ id <> "_o") mediaDir

exiv2Commands :: PhotoMeta -> [Text]
exiv2Commands PhotoMeta{..} =
  [ "-M", "set Iptc.Application2.Headline String " <> name
  -- , "-M", "set Iptc.Application2.Caption String " <> description
  , "-M", "set Exif.Photo.DateTimeOriginal Ascii " <> timestamp
  ]
  where
    timestamp =
      pack $
      formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") $
      unwrap date_taken


embedSidecar :: PhotoMeta -> FilePath -> IO (Maybe Int)
embedSidecar pm photoPath =
  try (sh $ inproc "exiv2" (exiv2Commands pm <> [showF photoPath]) empty) >>=
  \case
    Right _              -> return Nothing
    Left (ExitFailure n) -> return $ Just n
    Left ExitSuccess     -> error "embedSidecar: exception on exiv2 success"

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
  jsons <- liftIO $ forConcurrentlyN 20 sidecars parseSidecar
  res <- liftIO $ forConcurrentlyN 20 (take 20 $ catMaybes jsons) $ \sc -> (sc,) <$> do
    findFile mediaDir sc >>=
      \case
        Nothing -> return $ Just FileNotFound
        Just f -> embedSidecar sc f >>=
                  \case
                    Just err -> return $ Just $ Exiv2Failure err
                    Nothing  -> return Nothing
  print $ take 5 res
