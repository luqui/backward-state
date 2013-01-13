{-# LANGUAGE DoRec #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.BackwardState
-- Copyright   :  (c) Luke Palmer, 2013
-- License     :  BSD3
--
-- Maintainer  :  Luke Palmer <lrpalmer@gmail.com>
-- Stability   :  experimental
-- Portability :  needs DoRec
--
-- Backward state monad and transformer, in which @m >> n@ passes the incoming
-- state to n, then passes @n@\'s resulting state to @m@.  This can only work
-- lazily.
-----------------------------------------------------------------------------


module Control.Monad.Trans.BackwardState 
    ( BackwardState
    , runBackwardState
    , evalBackwardState
    , execBackwardState
    
    , BackwardStateT(..)
    , evalBackwardStateT
    , execBackwardStateT

    , get
    , put
    , modify
    )
where

import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor.Identity (Identity(..))
import Control.Monad.Fix (MonadFix(..))
import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad (ap, MonadPlus(..), liftM)
import Control.Arrow (first)

type BackwardState s = BackwardStateT s Identity

runBackwardState :: BackwardState s a -> s -> (a, s)
runBackwardState m = runIdentity . runBackwardStateT m

evalBackwardState :: BackwardState s a -> s -> a
evalBackwardState m = fst . runBackwardState m 

execBackwardState :: BackwardState s a -> s -> s
execBackwardState m = snd . runBackwardState m 

newtype BackwardStateT s m a = BackwardStateT { runBackwardStateT :: s -> m (a, s) }

evalBackwardStateT :: (Functor m) => BackwardStateT s m a -> s -> m a
evalBackwardStateT m = fmap fst . runBackwardStateT m

execBackwardStateT :: (Functor m) => BackwardStateT s m a -> s -> m s
execBackwardStateT m = fmap snd . runBackwardStateT m

get :: (Monad m) => BackwardStateT s m s
get = BackwardStateT $ \s -> return (s, s)

put :: (Monad m) => s -> BackwardStateT s m ()
put s' = BackwardStateT $ \s -> return ((), s')

modify :: (Monad m) => (s -> s) -> BackwardStateT s m ()
modify f = BackwardStateT $ \s -> return ((), f s)

instance (Functor m) => Functor (BackwardStateT s m) where
    fmap f b = BackwardStateT $ fmap (first f) . runBackwardStateT b

instance (Functor m, MonadFix m) => Applicative (BackwardStateT s m) where
    pure = return
    (<*>) = ap

instance (Functor m, MonadFix m, MonadPlus m) => Alternative (BackwardStateT s m) where
    empty = mzero
    (<|>) = mplus
 

instance (MonadFix m) => Monad (BackwardStateT s m) where 
    return x = BackwardStateT $ \s -> return (x, s)
    m >>= f = BackwardStateT $ \s'' -> do 
        rec (x, s) <- runBackwardStateT m s'
            (y, s') <- runBackwardStateT (f x) s''
        return (y, s)

instance MonadTrans (BackwardStateT s) where
    lift m = BackwardStateT $ \s -> liftM (flip (,) s) m



instance (MonadFix m, MonadPlus m) => MonadPlus (BackwardStateT s m) where
    mzero = BackwardStateT $ const mzero
    m `mplus` n = BackwardStateT $ \s -> runBackwardStateT m s `mplus` runBackwardStateT n s

instance (MonadFix m) => MonadFix (BackwardStateT s m) where
    mfix f = BackwardStateT $ \s -> mfix $ \ ~(a, _) -> runBackwardStateT (f a) s
        -- same as StateT... I think. Seems like it follows from the
        -- first MonadFix law (mfix (return . h) = return (fix h))

instance (MonadFix m, MonadIO m) => MonadIO (BackwardStateT s m) where
    liftIO = lift . liftIO
