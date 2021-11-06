{-# LANGUAGE TemplateHaskell, RecordWildCards #-}

module Migrations
    ( migrationMain
    , Config(..)
    ) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.FileEmbed (embedDir)
import Data.Function (on)
import qualified Data.List as L (sortBy)
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.Migration
    ( MigrationCommand(..)
    , MigrationContext(..)
    , MigrationResult(..)
    , runMigration
    )
import qualified System.Exit as Q
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

data Config =
    Config
        { databaseName :: T.Text
        , adminName :: T.Text
        , adminPassword :: T.Text
        }
  deriving (Show)

adminConnectionString :: Config -> BS.ByteString
adminConnectionString Config {..} = E.encodeUtf8 $
    "dbname=" <>
    databaseName <>
    " user=" <> adminName <> " password='" <> adminPassword <> "'"

migrationMain :: Config -> IO ()
migrationMain conf = do
    let conStr = adminConnectionString conf
    con <- PS.connectPostgreSQL conStr
    runMigrations con

sortedMigrations :: [(FilePath, BS.ByteString)]
sortedMigrations =
    let unsorted = $(embedDir "migrations")
     in L.sortBy (compare `on` fst) unsorted

runMigrations :: PS.Connection -> IO ()
runMigrations con =
    PS.withTransaction con $ do
        let defaultContext =
                MigrationContext
                    { migrationContextCommand =
                          MigrationInitialization
                    , migrationContextVerbose = True
                    , migrationContextConnection = con
                    }
            migrations =
                ("(init)", defaultContext) :
                [ ( k
                  , defaultContext
                        { migrationContextCommand =
                              MigrationScript k v
                        })
                | (k, v) <- sortedMigrations
                ]
        forM_ migrations $ \(_, migr) -> do
            res <- runMigration migr
            case res of
                MigrationSuccess -> pure ()
                MigrationError _ -> Q.exitFailure
