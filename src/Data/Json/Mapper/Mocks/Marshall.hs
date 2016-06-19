{-# Language FlexibleInstances #-}

module Data.Json.Mapper.Mocks.Marshall where

data JSVal

class FromJSVal a where
  fromJSVal :: JSVal -> IO (Maybe a)

class ToJSVal a where
  toJSVal :: a -> IO JSVal
  toJSValListOf :: [a] -> IO JSVal

instance ToJSVal [Char] where
  toJSVal = undefined
  toJSValListOf = undefined
