module Data.Json.Mapper.Mocks.Object where

import Data.Json.Mapper.Mocks.JSString
import Data.Json.Mapper.Mocks.Marshall

data Object

create :: IO Object
create = undefined

setProp :: JSString -> JSVal -> Object -> IO ()
setProp _ _ _ = undefined
