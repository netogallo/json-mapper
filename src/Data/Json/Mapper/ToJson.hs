{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts, CPP, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

module Data.Json.Mapper.ToJson where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Json.Mapper.Args
import GHC.Generics as G
import System.IO.Unsafe (unsafePerformIO)

#if defined(ghcjs_HOST_OS)
import GHCJS.Marshal
import GHCJS.Types (JSVal)
import qualified Data.JSString as S
import qualified JavaScript.Object as O
#else
import Data.Json.Mapper.Mocks.Marshall
import qualified Data.Json.Mapper.Mocks.JSString as S
import qualified Data.Json.Mapper.Mocks.Object as O
#endif


type MToJson v = ReaderT ToJsonArgs (State ToJsonContext) v

data JS

data HS

data SerializeArgs v = SerializeArgs (Maybe String)

trace x = seq (unsafePerformIO $ putStrLn x)

nextIx :: MToJson Int
nextIx = do
  ix <- _currentIx <$> lift get
  lift $ modify (over currentIx (+ 1))
  return ix

unsafeCreateObject v = unsafePerformIO $ O.create

unsafeToJSVal :: ToJSVal v => v -> JSVal
unsafeToJSVal = unsafePerformIO . toJSVal

unsafeSetProp :: S.JSString -> JSVal -> O.Object -> MToJson O.Object
unsafeSetProp s v o = seq (unsafePerformIO $ O.setProp s v o) $ return o

class ToJsonObjectMapper a where
  toJsonMapper :: ToJsonArgs -> a -> O.Object
  default toJsonMapper :: (Generic a, GToJsonObjectMapper (Rep a) ()) => ToJsonArgs -> a -> O.Object
  toJsonMapper args v = unsafePerformIO $ do
    obj <- O.create
    return $ evalState (runReaderT (runProcess obj) args) initConvert
    where
      runProcess obj = trace "step 0" $ gToJsonMapper obj () (G.from v)

class GToJsonObjectMapper f ctx | f -> ctx where
  gToJsonMapper :: O.Object -> ctx -> f a -> MToJson O.Object

instance GToJsonObjectMapper U1 () where
  gToJsonMapper o _ _ = return o

instance GToJsonObjectMapper a () => GToJsonObjectMapper (M1 C f a) () where
  gToJsonMapper o _ (M1 v) = trace "step1" $ gToJsonMapper o () v

instance GToJsonObjectMapper a () => GToJsonObjectMapper (M1 D f a) () where
  gToJsonMapper o _ (M1 v) = trace "step2" $ gToJsonMapper o () v

instance (GToJsonObjectMapper a (), GToJsonObjectMapper b ()) => GToJsonObjectMapper (a :*: b) () where
  gToJsonMapper o _ (a :*: b) = do
    o1 <- gToJsonMapper o () a
    gToJsonMapper o1 () b

instance (Selector s, GToJsonObjectMapper a (Maybe String)) => GToJsonObjectMapper (M1 S s a) () where
  gToJsonMapper o _ rep@(M1 v) =
    case selName rep of
    "" -> trace "setprop" $ gToJsonMapper o Nothing v
    sel -> trace "setprop1" $ gToJsonMapper o (Just sel) v

instance ToJSVal a =>  GToJsonObjectMapper (K1 i a) (Maybe String) where
  gToJsonMapper o ctx (K1 v) =
    case ctx of
    Nothing -> addToNextIx
    Just prop -> addAsProperty prop

    where
      addToNextIx = do
        i <- nextIx
        unsafeSetProp (S.pack $ show i) (unsafeToJSVal v) o
      addAsProperty prop = unsafeSetProp (S.pack prop) (unsafeToJSVal v) o

data V = V {a :: String, b :: String} deriving Generic

data V2 = V2 { c :: String, d :: V} deriving Generic

instance ToJsonObjectMapper V
-- instance ToJsonObjectMapper V2

val = gToJsonMapper undefined undefined (G.from $ V "" "kaiser")

-- val2 = gToJsonMapper undefined undefined (G.from $ V2 "z" (V "x" "y"))



