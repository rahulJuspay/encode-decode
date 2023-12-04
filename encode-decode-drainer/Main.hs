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
-- import Streamly.Internal.Data.Stream.IsStream.Transform (tapRate)
import qualified Streamly.Internal.Data.Stream.IsStream as S (chunksOfTimeout)
import Data.Aeson
import qualified Data.ByteString as BS
import System.IO ( hClose, openFile, IOMode(ReadWriteMode) )
import Control.Exception (evaluate)
import Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Data.Stream.Prelude as Stream
import Streamly.Data.Stream.Prelude (MonadAsync, Stream)
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Data.Fold as Fold
-- import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S
import Streamly.Internal.Control.Concurrent (MonadAsync)
import Control.Monad.Catch (MonadCatch, throwM)
import qualified Streamly.Internal.Data.Fold as S (maximum, unzip, lmap, toMap)
import Data.Map as DM
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Stream.IsStream.Common as SC (map)
import qualified Kafka.Consumer as KC
import qualified Kafka.Producer as  KP
import Prelude hiding (lookup)
import Control.Error.Util (hush)
import Control.Exception (evaluate)
import Control.Lens.Combinators
import Control.Lens.Operators
import Control.Monad (void)
import Control.Monad.Catch (bracket)
import Data.Either
import Data.Text.Strict.Lens
import Options.Applicative ( execParser, info )
import qualified Data.Text.Lazy as TL
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S
import qualified Data.Map.Strict as Map
import qualified Data.List as DL
import Control.Arrow
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.ByteString.Lazy as L
-- import qualified Streamly.Internal.Data.Stream.IsStream Transform

tapRate :: (MonadAsync m) => Double -> (Int -> m b) -> Stream m a -> Stream m a
tapRate interval action =
  Stream.parTapCount (const True) report
  where
  report = Stream.fold (Fold.drainMapM action) . rate
  rate = Stream.rollingMap2 (flip (-)) . Stream.delayPost interval

tapRateSerialT ::
       (S.IsStream t, MonadAsync m, MonadCatch m)
    => Double
    -> (Int -> m b)
    -> t m a
    -> t m a
tapRateSerialT n f xs = S.fromStreamD $ tapRate n f $ S.toStreamD xs

main :: IO ()
main = do
  CommandLineArgs
    { prometheusSettings
      , kafkaConsumerParameters
      , sinkKafkaParameters
    } <- execParser (info cmdArgsParser mempty)
  startMetricsApp (port prometheusSettings)
  logsCounter <- register $ counter (Info "log" "Total logs")
  let
    mainLoop (kc, kp) =
        S.mapM_ (\logList -> do
          print $ Prelude.length logList
          ) $
         tapRateSerialT
          (fromIntegral $ updateInterval prometheusSettings)
          (updateMetrics logsCounter (updateInterval prometheusSettings))
          (messagesFromKafka kc)
         & S.chunksOfTimeout 7000000 5 Fold.toList
  bracket
      ((,)
        <$> (KC.newConsumer (consumerProperties kafkaConsumerParameters) (KC.topics (topics kafkaConsumerParameters))
               >>= evaluate . fromRight (error "Unable to create consumer"))
        <*> (KP.newProducer (producerProperties sinkKafkaParameters)
               >>= evaluate . fromRight (error "Unable to create producer")))
      (\(kc, kp) -> do
        putStrLn "Encountered Exception. Entering Bracket"
        KP.flushProducer kp
        putStrLn "Flush Producer completed"
        KP.closeProducer kp
        putStrLn "Closed Producer"
        KC.closeConsumer kc
        putStrLn "Closed Consumer")
      mainLoop
  where
  messagesFromKafka kc = S.repeatM (KC.pollMessage kc (KC.Timeout 10))
  -- messagesFromStdin =
  --   S.repeatM $ BS.getLine
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
