{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts, CPP, MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

module Data.Json.Mapper.ToJson where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Json.Mapper.Args
import GHC.Generics as G
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

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
      runProcess obj = gToJsonMapper obj () (G.from v)

class GToJsonObjectMapper f ctx | f -> ctx where
  gToJsonMapper :: O.Object -> ctx -> f a -> MToJson O.Object

instance GToJsonObjectMapper U1 () where
  gToJsonMapper o _ _ = return o

instance GToJsonObjectMapper a () => GToJsonObjectMapper (M1 C f a) () where
  gToJsonMapper o _ (M1 v) = gToJsonMapper o () v

instance GToJsonObjectMapper a () => GToJsonObjectMapper (M1 D f a) () where
  gToJsonMapper o _ (M1 v) = gToJsonMapper o () v

instance (GToJsonObjectMapper a (), GToJsonObjectMapper b ()) => GToJsonObjectMapper (a :*: b) () where
  gToJsonMapper o _ (a :*: b) = do
    o1 <- gToJsonMapper o () a
    gToJsonMapper o1 () b

instance (Selector s, GToJsonObjectMapper a (Maybe String)) => GToJsonObjectMapper (M1 S s a) () where
  gToJsonMapper o _ rep@(M1 v) =
    case selName rep of
    "" -> gToJsonMapper o Nothing v
    sel -> gToJsonMapper o (Just sel) v

instance GToJsonObjectMapper (K1 i Int) (Maybe String) where
  gToJsonMapper = jsValHelper

instance GToJsonObjectMapper (K1 i Float) (Maybe String) where
  gToJsonMapper = jsValHelper

instance GToJsonObjectMapper (K1 i Double) (Maybe String) where
  gToJsonMapper = jsValHelper

instance GToJsonObjectMapper (K1 i Char) (Maybe String) where
  gToJsonMapper = jsValHelper

instance GToJsonObjectMapper (K1 i Bool) (Maybe String) where
  gToJsonMapper = jsValHelper

instance GToJsonObjectMapper (K1 i String) (Maybe String) where
  gToJsonMapper = jsValHelper
  
jsValHelper :: (ToJSVal a) => O.Object -> (Maybe String) -> K1 i a b -> MToJson O.Object
jsValHelper o ctx (K1 v) =
    case ctx of
    Nothing -> addToNextIx
    Just prop -> addAsProperty prop

    where
      value = (unsafeToJSVal v)
      addToNextIx = do
        i <- nextIx
        unsafeSetProp (S.pack $ show i) value o
      addAsProperty prop = unsafeSetProp (S.pack prop) value o

instance {-# OVERLAPS #-} ToJsonObjectMapper v =>  GToJsonObjectMapper (K1 i v) (Maybe String) where
  gToJsonMapper o ctx (K1 v) = do
    args <- ask
    let value = unsafeCoerce $ toJsonMapper args v
    case ctx of
      Nothing -> addToNextIx value
      Just prop -> addAsProperty prop value
    where
      addToNextIx value = do
        i <- nextIx
        unsafeSetProp (S.pack $ show i) value o
      addAsProperty prop value = unsafeSetProp (S.pack prop) value o      

data V = V {a :: String, b :: String} deriving Generic

data V2 = V2 { c :: String, d :: V} deriving Generic

instance ToJsonObjectMapper V
instance ToJsonObjectMapper V2

val = gToJsonMapper undefined undefined (G.from $ V "" "kaiser")

val2 = gToJsonMapper undefined undefined (G.from $ V2 "z" (V "x" "y"))



