{-# LANGUAGE TemplateHaskell #-}

module Main where

import           ClassyPrelude           hiding ( FilePath )
import           Control.Monad.Logger
import           Data.Aeson                    as A
                                         hiding ( Options )
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

newtype Tag = Tag { unwrap :: Text }
  deriving Show

instance FromJSON Tag where
  parseJSON = withObject "Tag" $ \o -> Tag <$> (o .: "tag")

newtype Album = Album { unwrap :: Text }
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
      "public"          -> return Public
      "private"         -> return Private
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

optParser :: Parser Options
optParser =
  Options
    <$> argPath "meta"  "Directory containing photo_*.json files"
    <*> argPath "media" "Directory with media files"

parseSidecar :: MonadIO m => FilePath -> m (Maybe PhotoMeta)
parseSidecar jf = do
  res <- liftIO $ ClassyPrelude.readFile $ encodeString jf
  return $ decode $ fromStrict res

main :: IO ()
main = runStdoutLoggingT $ do
  Options {..} <- options "Process Flickr takeaway files" optParser
  files        <- flip Turtle.fold Fold.list $ do
    pushd metaDir
    Turtle.find (contains "photo_" *> suffix ".json") =<< pwd
  $logInfo $ format (d % " sidecar .json files found") (length files)
  jsons <- mapConcurrently parseSidecar files
  print $ length $ filter ((/= Public) . privacy) $ catMaybes jsons
  print $ headMay jsons
