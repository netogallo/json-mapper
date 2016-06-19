{-# LANGUAGE TemplateHaskell #-}

module Data.Json.Mapper.Args where

import qualified Control.Lens as L

data Args

data Context = Context

data ToJsonArgs = ToJsonArgs {
  _baseArgs :: Args
  }

L.makeLenses ''ToJsonArgs

data ToJsonContext = ToJsonContext {
  _baseContext :: Context
  ,_currentIx :: Int
  }

initConvert :: ToJsonContext
initConvert = ToJsonContext Context 0

L.makeLenses ''ToJsonContext
