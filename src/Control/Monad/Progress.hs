--------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Progress
-- Copyright   :  (c) 2016 Chordify B.V., Groningen
-- License     :  LGPL-3
--
-- Maintainer  :  haskelldevelopers@chordify.net
-- Stability   :  stable
-- Portability :  non-portable
--
-- Summary: Functionality for reporting function progress.
--
--------------------------------------------------------------------------------

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Progress (
  -- * Data types
  WithProgress,

  -- * Exported functions
  runWithProgress,
  runWithPercentage,
  withProgressFromList,
  withProgressM,
  withProgressA,
  withProgressFinally,
  setWeight,
  setWeights,
  printComponentTime,
  measureComponentTimes,

  -- * Combining progress
  (C.>>>),
  Arrow (..),
  ArrowChoice (..)
  ) where

import Control.DeepSeq
import Control.Monad        ( forM_, when )
import Control.Monad.Trans  ( MonadIO (..) )
import Control.Monad.Writer ( WriterT, execWriterT, tell, lift )
import Control.Monad.Catch  ( finally, MonadMask )
import Control.Arrow        ( Arrow (..), ArrowChoice (..) )
import qualified Control.Category as C

import Data.List            ( genericLength )
import Data.IORef           ( newIORef, atomicModifyIORef', readIORef )
import Data.Time            ( getCurrentTime, diffUTCTime )

--------------------------------------------------------------------------------  
-- Data type
--------------------------------------------------------------------------------

data WithProgress m a b where
  Id            :: WithProgress m a a
  WithProgressM :: ((Double -> m ()) -> a -> m b)                  -> WithProgress m a b
  Combine       :: WithProgress m b c        -> WithProgress m a b -> WithProgress m a c
  Finally       :: MonadMask m => (a -> m c) -> WithProgress m a b -> WithProgress m a b
  SetWeight     :: Double                    -> WithProgress m a b -> WithProgress m a b
  First         ::                              WithProgress m a b -> WithProgress m (a,c) (b,c)
  Second        ::                              WithProgress m a b -> WithProgress m (c,a) (c,b)
  LeftA         ::                              WithProgress m a b -> WithProgress m (Either a c) (Either b c)

instance C.Category (WithProgress m) where
  id  = Id
  (.) = Combine

instance Monad m => Arrow (WithProgress m) where
  arr fun = WithProgressM (\report b -> fun b <$ report 1) -- haskell is lazy so this completes instantly
  first   = First
  second  = Second
  f *** g = First f C.>>> Second g
  f &&& g = WithProgressM (\_ b -> return (b,b)) C.>>> f *** g

instance Monad m => ArrowChoice (WithProgress m) where
  left = LeftA
  right f = mirror C.>>> left f C.>>> mirror where
    mirror :: WithProgress m (Either a b) (Either b a)
    mirror = WithProgressM $ \_ x -> case x of
      Left y  -> return $ Right y
      Right y -> return $ Left y

--------------------------------------------------------------------------------  
-- Functionality
--------------------------------------------------------------------------------

-- | From a function @a -> [b]@, construct a function that can reports its progress.
--   Important remark: The resulting list must __not__ be lazily constructed, it
--   should be immediately possible to compute the length of this list in order
--   for 'withProgressFromList' to work correctly.
withProgressFromList :: forall a b m. (Monad m, NFData b) => (a -> [b]) -> WithProgress m a [b]
withProgressFromList f = WithProgressM ret where
  ret :: (Double -> m ()) -> a -> m [b]
  ret report input = do
    -- Start with reporting 0 progress
    report 0
    
    -- First construct output list to find its length
    let output = f input
    let len :: Double
        len = genericLength output

    -- Now force evaluation of every element and report progress
    forM_ (zip [1..] output) $ \(n,el) -> do
      () <- deepseq el $ return ()
      report $ n / len

    -- Corner-case, make sure we always report 1 even when list is empty
    when (null output) $ report 1

    -- Return the output
    return output

-- | Lift an atomic monadic function to the progress reporting arrow. The result
--   is evaluated with 'deepseq' before being returned. Note that this function
--   should not be used for time consuming functions, use 'withProgressM' for
--   that instead to report intermediate progress!
withProgressA :: (Monad m, NFData b) => (a -> m b) -> WithProgress m a b
withProgressA f = WithProgressM $ \report a -> do
  report 0
  r <- f a
  report 1
  r `deepseq` return r

-- | Attach an exception handler from the 'Control.Monad.Catch' class
--   to this pipeline, which is executed when the pipeline completes
--   or an exception occurs.
withProgressFinally :: MonadMask m => WithProgress m a b -> (a -> m c) -> WithProgress m a b
withProgressFinally = flip Finally

-- | Set the weight of a pipeline element (default is 1).
setWeight :: Double -> WithProgress m a b -> WithProgress m a b
setWeight = SetWeight

-- | Set the weight of all components in the given pipeline. Note that the length
--   of the list must match the number of components exactly. This function is
--   used in combination with 'measureComponentTimes' to estimate the runtime
--   of each component.
--   This function erases all weights that already exist.
setWeights :: [Double] -> WithProgress m a b -> WithProgress m a b
setWeights times wp = case f times wp of
  ([], p') -> p'
  _        -> error "setWeights: The number of times is bigger than the number of components"
  where
    f :: [Double] -> WithProgress m a b -> ([Double], WithProgress m a b)
    f    ts   Id               = (ts, Id)
    f    []  (WithProgressM _) = error "setWeights: The number of times is smaller than the number of components"
    f (t:ts) (WithProgressM p) = (ts, SetWeight t (WithProgressM p))
    f    ts  (SetWeight _ p)   = f ts p
    f    ts  (Combine q p)     = let (ts',p')  = f ts p
                                     (ts'',q') = f ts' q
                                 in  (ts'', Combine q' p')
    f    ts  (Finally g p)     = let (ts',p')  = f ts p
                                 in  (ts', Finally g p')
    f    ts  (First p)         = let (ts',p')  = f ts p
                                 in  (ts',First p')
    f    ts  (Second p)        = let (ts',p')  = f ts p
                                 in  (ts',Second p')
    f    ts  (LeftA p)         = let (ts',p')  = f ts p
                                 in  (ts',LeftA p')

-- | Construct a function that reports its own progress. This function must call
--   the given function to report progress as a fraction between 0 and 1.
withProgressM :: ((Double -> m ()) -> a -> m b) -> WithProgress m a b
withProgressM f = WithProgressM f

-- | Run a computation with progress reporting. The given function will be called
--   each time the progress is updated, and the number is always between 0 and 1.
runWithProgress :: Monad m => WithProgress m a b -> (Double -> m ()) -> a -> m b
runWithProgress Id r a = r 1 >> return a
runWithProgress p  r a = runWithProgress' p (r . (/w)) a where
  w = getWeight p

-- | Run a computation with progress reporting. The given function will be called
--   at most once per percentage, which is a number between 0 and 100.
runWithPercentage :: MonadIO m => WithProgress m a b -> (Int -> m ()) -> a -> m b
runWithPercentage Id r a = r 100 >> return a
runWithPercentage p  r a = do
  let w = getWeight p
  r 0
  prevR <- liftIO $ newIORef 0
  let report d = do
        let new = floor $ (/w) $ (*100) d
        isNew <- liftIO $ atomicModifyIORef' prevR $ \prev -> (new, prev /= new)
        when isNew $ r new
  ret <- runWithProgress' p report a
  final <- liftIO $ readIORef prevR
  when (final /= 100) $ report 100
  return ret

-- | Internal function for actually running the computation, which does not do the
--   scaling of the total weight, so the reported number is between 0 and getWeight p
runWithProgress' :: Monad m => WithProgress m a b -> (Double -> m ()) -> a -> m b
runWithProgress' Id                _ a = return a
runWithProgress' (WithProgressM p) r a = p r a
runWithProgress' (SetWeight w p)   r a = runWithProgress' p (r . (*w) . (/wp)) a where
  wp = getWeight p
runWithProgress' (Combine q p)     r a = runWithProgress' p r a >>= runWithProgress' q (r . (+wp)) where
  wp = getWeight p
runWithProgress' (Finally g p)     r a = runWithProgress' p r a `finally` g a
runWithProgress' (First p)         r (a,c) = runWithProgress' p r a >>= \b -> return (b,c)
runWithProgress' (Second p)        r (c,a) = runWithProgress' p r a >>= \b -> return (c,b)
runWithProgress' (LeftA p)         r (Left a) = runWithProgress' p r a >>= \b -> return (Left b)
runWithProgress' (LeftA _)         r (Right b) = r 1 >> return (Right b)

-- | Run the computation with progress reporting, and measure the time of each
--   component and print that to the screen. This function can be used to decide
--   what the weight of each component should be.
printComponentTime :: MonadIO m => WithProgress m a b -> a -> m b
printComponentTime c a = printTime >> f c a >>= \r -> printTime >> return r where
  f :: MonadIO m => WithProgress m a b -> a -> m b
  f Id                a' = return a'
  f (SetWeight _ p)   a' = f p a'
  f (WithProgressM p) a' = p (const $ return ()) a'
  f (Combine q p)     a' = f p a' >>= \b -> printTime >> f q b
  f (Finally g p)     a' = f p a' `finally` g a'
  f (First p)         (a',c') = f p a' >>= \b -> return (b,c')
  f (Second p)        (c',a') = f p a' >>= \b -> return (c',b)
  f (LeftA p)         (Left a') = f p a' >>= \b -> return (Left b)
  f (LeftA _)         (Right _) = error "printComponentTime: Empty branch of ArrowChoice reached, this should not be the case with time measurements"

-- | Print the current time to stdout
printTime :: MonadIO m => m ()
printTime = liftIO (getCurrentTime >>= print)

-- | Measure the time of all components in a pipeline.
measureComponentTimes :: MonadIO m => WithProgress m a b -> a -> m [Double]
measureComponentTimes c a = execWriterT $ f c a where
  f :: MonadIO m => WithProgress m a b -> a -> WriterT [Double] m b
  f Id                a' = return a'
  f (SetWeight _ p)   a' = f p a'
  f (WithProgressM p) a' = do
    start <- liftIO getCurrentTime
    b <- lift $ p (const $ return ()) a'
    end <- liftIO getCurrentTime
    tell [fromRational $ toRational $ diffUTCTime end start]
    return b
  f (Combine q p)     a' = f p a' >>= \b -> f q b
  f (Finally _ p)     a' = f p a'
  f (First p)         (a',c') = f p a' >>= \b -> return (b,c')
  f (Second p)        (c',a') = f p a' >>= \b -> return (c',b)
  f (LeftA p)         (Left a') = f p a' >>= \b -> return (Left b)
  f (LeftA _)         (Right _) = error "measureComponentTimes: Empty branch of ArrowChoice reached, this should not be the case with time measurements"

-- | Get the weight of a computation with progress
getWeight :: WithProgress m a b -> Double
getWeight Id                = 0
getWeight (WithProgressM _) = 1
getWeight (Combine p q)     = getWeight p + getWeight q
getWeight (Finally _ p)     = getWeight p
getWeight (SetWeight w _)   = w
getWeight (First p)         = getWeight p
getWeight (Second p)        = getWeight p
getWeight (LeftA p)         = getWeight p
