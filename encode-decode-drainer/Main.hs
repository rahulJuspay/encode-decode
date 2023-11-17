{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent ( forkIO )
import Control.Monad
import Control.Monad.Catch (bracket)
import Data.Function
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.Prometheus ( metricsApp )
import Options.Applicative (execParser, info)
import ProgramOptions
import Prometheus
import qualified Streamly.Prelude as S
import Streamly.Internal.Data.Stream.IsStream.Transform (tapRate)
import qualified Streamly.Internal.Data.Stream.IsStream as S (chunksOfTimeout)
import Data.Aeson
import qualified Data.ByteString as BS
import System.IO ( hClose, openFile, IOMode(ReadWriteMode) )
import Control.Exception (evaluate)
import Streamly.Internal.Data.Fold as Fold

main :: IO ()
main = do
  CommandLineArgs
    { prometheusSettings
    } <- execParser (info cmdArgsParser mempty)
  startMetricsApp (port prometheusSettings)
  logsCounter <- register $ counter (Info "log" "Total logs")
  let
    mainLoop =
        S.mapM_ (\logList -> do
          print $ Prelude.length logList
          ) $
         tapRate
          (fromIntegral $ updateInterval prometheusSettings)
          (updateMetrics logsCounter (updateInterval prometheusSettings))
          (messagesFromStdin)
         & S.chunksOfTimeout 70000 5 Fold.toList
  mainLoop
  where
  messagesFromStdin =
    S.repeatM $ BS.getLine
    -- BS.getLine
    -- BS.hGetLine hndl
  updateMetrics counter_ interval i = do
    putStrLn $
      "Received " <> show i <> " messages in " <> show interval <> " seconds"
    unsafeAddCounter counter_ (fromIntegral i)
  startMetricsApp metricsPort =
    void $
      forkIO $
        run metricsPort metricsApp
