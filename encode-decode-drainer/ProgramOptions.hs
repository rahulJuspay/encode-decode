{-# LANGUAGE OverloadedStrings #-}

module ProgramOptions where

import           Options.Applicative
import Kafka.Consumer
import qualified Kafka.Producer as KP
import qualified Data.Map.Strict as Map
import qualified Data.Text as DT ( breakOn, splitOn, Text, tail)

data PrometheusSettings
  = PrometheusSettings
    { updateInterval :: Int
    , port :: Int }

data KafkaProducerParameters
  = KafkaProducerParameters
  { sinkTopic :: KP.TopicName
  , producerProperties :: KP.ProducerProperties }

data KafkaConsumerParameters
  = KafkaConsumerParameters
  { topics :: [TopicName]
  , consumerProperties :: ConsumerProperties }

data CommandLineArgs =
  CommandLineArgs
  { prometheusSettings :: PrometheusSettings
    , kafkaConsumerParameters :: KafkaConsumerParameters
    , sinkKafkaParameters :: KafkaProducerParameters
  }

cmdArgsParser :: Parser CommandLineArgs
cmdArgsParser =
  CommandLineArgs
  <$> parsePrometheusSettings
  <*> parseKafkaConsumerParameters
  <*> parseKafkaProducerParameters
  -- <*> parseFileName
  where
  parsePrometheusSettings =
    PrometheusSettings
      <$> option auto (long "metrics-update-interval")
      <*> option auto (long "prometheus-port")
  parseKafkaProducerParameters =
    KafkaProducerParameters <$> parseSinkTopicName <*> parseProducerProps
  parseSinkTopicName =
      TopicName <$>
          strOption (long "sink-topic" <> metavar "TOPIC")
  parseProducerProps =
      (<>) <$> parseSinkBrokers <*> parseSinkExtraProps
  parseSinkBrokers =
      KP.brokersList <$>
          (many $
          BrokerAddress <$>
              strOption (long "sink-kafka-brokers" <> metavar "BROKERS" <> help "Kafka host:port"))
  parseSinkExtraProps =
      KP.extraProps . Map.fromList . fmap (fmap DT.tail . DT.breakOn "=") <$>
          many
          (strOption $ long "producer-option" <> metavar "PRODUCER_PROP")
  parseKafkaConsumerParameters =
      KafkaConsumerParameters <$> parseTopicNames <*> parseConsumerProps
  parseConsumerProps =
      (\a b c -> a <> b <> c <>  noAutoCommit)
          <$> parseConsumerGroup
          <*> parseBrokers
          <*> parseExtraProps
  parseConsumerGroup =
      fmap (groupId . ConsumerGroupId) $
          strOption $
          long "consumer-group-id" <> metavar "GROUP-ID"
  parseBrokers =
      brokersList . fmap BrokerAddress . DT.splitOn "," <$>
          strOption (long "kafka-brokers" <> metavar "BROKERS" <> help "Comma separated list of form host:port")
  parseTopicNames =
          fmap
              (fmap TopicName)
              (many $ strOption (long "topics" <> metavar "TOPIC"))
  parseExtraProps =
      foldMap (uncurry extraProp . fmap DT.tail . DT.breakOn "=") <$>
          many
          (strOption $ long "consumer-option" <> metavar "PROP")
  -- parseFileName =
  --     strOption (long "file" <> metavar "DIMENSIONS" <> help "log file")