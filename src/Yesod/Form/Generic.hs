module Yesod.Form.Generic where

import Yesod.Core
import Yesod.Form
import Data.Text (Text)
import Control.Monad
import Control.Applicative
import Data.Monoid
import Control.Monad.Trans.RWS (ask, get, put, runRWST, tell, evalRWST, local)
import Data.Maybe
import qualified Data.Map as Map

newtype GForm w m a = GForm 
  { unGForm :: (HandlerSite m, [Text])
            -> Maybe (Env, FileEnv)
            -> Ints
            -> m (FormResult a, w, Ints, Enctype)
  }

instance Monad m => Functor (GForm w m) where
  fmap f (GForm a) = GForm $ \x y z -> liftM go $ a x y z
    where go (w, x, y, z) = (fmap f w, x, y, z)

instance (Monad m, Monoid w) => Applicative (GForm w m) where
  pure x = GForm $ const $ const $ \ints -> return (FormSuccess x, mempty, ints, mempty)
  (GForm f) <*> (GForm g) = GForm $ \mr env ints -> do
    (a, b, ints', c) <- f mr env ints
    (x, y, ints'', z) <- g mr env ints'
    return (a <*> x, b <> y, ints'', c <> z)

mghelper :: MonadHandler m
         => Enctype 
         -> ([Text] -> [FileInfo] -> m (FormResult a)) -- ^ parser
         -> (Text -> [Text] -> FormResult a -> w) -- ^ function for building output, needs name and vals
         -> MForm m (FormResult a, w)
mghelper enctype parse buildOutput = do
  tell enctype
  mp <- askParams
  name <- newFormIdent 
  case mp of
    Nothing -> return (FormMissing, buildOutput name [] FormMissing)
    Just p -> do
      mfs <- askFiles
      let mvals = fromMaybe [] $ Map.lookup name p
          files = fromMaybe [] $ mfs >>= Map.lookup name
      res <- lift $ parse mvals files
      return (res, buildOutput name mvals res)

ghelper :: MonadHandler m
        => Enctype 
        -> ([Text] -> [FileInfo] -> m (FormResult a)) -- ^ parser
        -> (Text -> [Text] -> FormResult a -> w) -- ^ function for building output, needs name
        -> GForm w m a
ghelper a b c = formToGForm (mghelper a b c)

formToGForm :: (HandlerSite m ~ site, Monad m)
            => MForm m (FormResult a, w)
            -> GForm w m a
formToGForm form = GForm $ \(site, langs) env ints -> do
  ((a, w), ints', enc) <- runRWST form (env, site, langs) ints
  return (a, w, ints', enc)

gFormToForm :: (Monad m, HandlerSite m ~ site)
            => GForm w m a
            -> MForm m (FormResult a, w)
gFormToForm (GForm gform) = do
  ints <- get
  (env, site, langs) <- ask
  (a, w, ints', enc) <- lift $ gform (site, langs) env ints
  put ints'
  tell enc
  return (a, w)

monoidToGForm :: Monad m => w -> GForm w m ()
monoidToGForm w = formToGForm $ return (FormSuccess (), w)


