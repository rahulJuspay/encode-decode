{-# LANGUAGE OverloadedStrings #-}

module ProgramOptions where

import           Options.Applicative

data PrometheusSettings
  = PrometheusSettings
    { updateInterval :: Int
    , port :: Int }

data CommandLineArgs =
  CommandLineArgs
  { prometheusSettings :: PrometheusSettings
   , fName :: FilePath
   }

cmdArgsParser :: Parser CommandLineArgs
cmdArgsParser =
  CommandLineArgs
  <$> parsePrometheusSettings
  <*> parseFileName
  where
  parsePrometheusSettings =
    PrometheusSettings
      <$> option auto (long "metrics-update-interval")
      <*> option auto (long "prometheus-port")
  parseFileName =
      strOption (long "file" <> metavar "DIMENSIONS" <> help "log file")