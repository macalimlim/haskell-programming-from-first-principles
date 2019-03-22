{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Chapter26 where

import Control.Applicative        (Alternative (empty, (<|>)), Applicative,
                                   pure, (<*>))
import Control.Monad              (Monad, guard, return, (>>=))
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Class  (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Control.Monad.Trans.Reader (Reader, reader)
import Data.Bool                  (Bool)
import Data.Either                (Either (Left, Right), either)
import Data.Function              (const, ($), (.))
import Data.Functor               (Functor, fmap, (<$>))
import Data.Functor.Identity      (Identity)
import Data.Int                   (Int)
import Data.List                  (elem, (++))
import Data.Maybe                 (Maybe (Just, Nothing))
import Data.Monoid                ((<>))
import Data.String                (String)
import GHC.Num                    (Num, subtract, (+), (-))
import System.IO                  (IO, getLine, putStrLn)
import Text.Show                  (Show, show)

newtype EitherT e m a = EitherT
                      { runEitherT :: m (Either e a)
                      }

{-

Exercises: EitherT

1. Write the Functor instance for EitherT:

instance Functor m => Functor (EitherT e m) where
  fmap = undefined

-}

instance Functor m => Functor (EitherT e m) where
  fmap :: Functor m => (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT m) = EitherT $ (fmap . fmap) f m

{-

2. Write the Applicative instance for EitherT:

instance Applicative m => Applicative (EitherT e m) where
  pure    = undefined
  f <*> a = undefined

-}

instance Applicative m => Applicative (EitherT e m) where
  pure :: Applicative m => a -> EitherT e m a
  pure = EitherT . pure . pure

  (<*>) :: Applicative m => EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT f) <*> (EitherT a) = EitherT $ (<*>) <$> f <*> a

{-

3. Write the Monad instance for EitherT:

instance Monad m => Monad (EitherT e m) where
  return  = pure
  v >>= f = undefined

-}

instance Monad m => Monad (EitherT e m) where
  return :: Monad m => a -> EitherT e m a
  return = pure

  (>>=) :: Monad m => EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT m) >>= f = EitherT $ m >>= either g h
    where g         = return . Left
          h         = runEitherT . f

{-

4. Write the swapEitherT helper function for EitherT.

-- transformer version of swapEither.
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT = undefined

Hint: write swapEither first, then swapEitherT in terms of the former.

-}

swapEither :: Either e a -> Either a e
swapEither (Left x)  = Right x
swapEither (Right x) = Left x

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT m) = EitherT $ fmap swapEither m

{-

5. Write the transformer variant of the either catamorphism.

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT = undefined

-}

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT m) = m >>= either f g


newtype StateT s m a = StateT
                     { runStateT :: s -> m (a, s)
                     }

{-

Exercises: StateT

If you’re familiar with the distinction, you’ll be implementing the
strict variant of StateT here. To make the strict variant, you don’t have
to do anything special. Write the most obvious thing that could work.

The lazy (lazier, anyway) variant is the one that involves adding a bit
extra. We’ll explain the difference in the chapter on nonstrictness.

1. You’ll have to do the Functor and Applicative instances first, be-
cause there aren’t Functor and Applicative instances ready to go
for the type Monad m => s -> m (a, s).

instance (Functor m) => Functor (StateT s m) where
  fmap f m = undefined

-}

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT m) = StateT g
    where g s       = h <$> m s
          h (a, s') = (f a, s')

{-

2. As with Functor, you can’t cheat and reuse an underlying Applicative
instance, so you’ll have to do the work with the s -> m (a, s)
type yourself.

instance (Monad m) => Applicative (StateT s m) where
  pure  = undefined
  (<*>) = undefined

Also note that the constraint on m is not Applicative as you expect,
but rather Monad. This is because you can’t express the
order-dependent computation you’d expect the StateT Applicative
to have without having a Monad for m. To learn more, see this
Stack Overflow question1 about this issue. Also see this Github
issue2 on the NICTA Course Github repository. Beware! The
NICTA course issue gives away the answer. In essence, the issue
is that without Monad, you’re feeding the initial state to each
computation in StateT rather than threading it through as you
go. This is a general pattern contrasting Applicative and Monad
and is worth contemplating.

-}

instance (Monad m) => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure x      = StateT f
    where f s = pure (x, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT f) <*> (StateT m) = StateT g
    where g s               = do
            (f', s') <- f s
            h f' <$> m s'
          h f'' (a, s)      = (f'' a, s)

{-

3. The Monad instance should look fairly similar to the Monad instance
you wrote for ReaderT.

instance (Monad m) => Monad (StateT s m) where
  return    = pure
  sma >>= f = undefined

-}

instance (Monad m) => Monad (StateT s m) where
  return :: a -> StateT s m a
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT m) >>= f = StateT g
    where g s      = do
            (a, s') <- m s
            runStateT (f a) s'

{-

Exercise: Wrap It Up

Turn readerUnwrap from the previous example back into embedded
through the use of the data constructors for each transformer.

-- Modify it to make it work.
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = ??? (const (Right (Just 1)))

-}

newtype MaybeT m a = MaybeT
                   { runMaybeT :: m (Maybe a)
                   }

instance (Functor m) => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT m) = MaybeT $ (fmap . fmap) f m

instance (Applicative m) => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure =  MaybeT . pure . pure

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (MaybeT f) <*> (MaybeT m) = MaybeT $ (<*>) <$> f <*> m

instance (Monad m) => Monad (MaybeT m) where
  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT m) >>= f = MaybeT $ do
    m' <- m
    case m' of
      Nothing         -> return Nothing
      Just a          -> runMaybeT $ f a

newtype ReaderT r m a = ReaderT
                      { runReaderT :: r -> m a
                      }

instance (Functor m) => Functor (ReaderT r m) where
  fmap :: (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT g) = ReaderT h
    where h r        = f <$> g r

instance (Applicative m) => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure a      = ReaderT f
    where f _ = pure a

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (ReaderT f) <*> (ReaderT g) = ReaderT h
    where h r                 = f r <*> g r

instance (Monad m) => Monad (ReaderT r m) where
  return :: a -> ReaderT r m a
  return = pure

  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT f) >>= g = ReaderT h
    where h r       = do
            a <- f r
            (runReaderT $ g a) r

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = (MaybeT . ExceptT . ReaderT) ((const . return . return . return) 1)

{-

Exercises: Lift More

Keep in mind what these are doing, follow the types, lift till you drop.

1. You thought you were done with EitherT.

instance MonadTrans (EitherT e) where
  lift = undefined

-}

instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift = EitherT . fmap return

{-

2. Or StateT. This one’ll be more obnoxious. It’s fine if you’ve seen
this before.

instance MonadTrans (StateT s) where
  lift = undefined

-}

instance MonadTrans (StateT s) where
  lift :: Monad m => m a -> StateT s m a
  lift          = StateT . f
    where f m s = do
            a <- m
            return (a, s)

{-

Exercises: Some Instances

1. MaybeT

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = undefined

-}

instance MonadTrans MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift m = MaybeT $ Just <$> m

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO :: Monad m => IO a -> MaybeT m a
  liftIO = lift . liftIO

{-

2. ReaderT

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = undefined

-}

instance MonadTrans (ReaderT r) where
  lift :: Monad m => m a -> ReaderT r m a
  lift m      = ReaderT f
    where f _ = m

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO :: Monad m => IO a -> ReaderT r m a
  liftIO = lift . liftIO

{-

3. StateT

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = undefined

Hint: your instances should be simple.

-}

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO :: Monad m => IO a -> StateT s m a
  liftIO = lift . liftIO


{-

Chapter Exercises

Write the code

1. rDec is a function that should get its argument in the context of
Reader and return a value decremented by one.

rDec :: Num a => Reader a a
rDec = undefined

Prelude> import Control.Monad.Trans.Reader
Prelude> runReader rDec 1
0
Prelude> fmap (runReader rDec) [1..10]
[0,1,2,3,4,5,6,7,8,9]

Note that “Reader” from transformers is ReaderT of Identity and
that runReader is a convenience function throwing away the
meaningless structure for you. Play with runReaderT if you like.

-}

rDec :: Num a => Reader a a
rDec          = reader dec
  where dec x = x - 1

{-

2. Once you have an rDec that works, make it and any inner lamb-
das pointfree if that’s not already the case.

-}

rDec' :: Num a => Reader a a
rDec' = reader $ subtract 1

{-

3. rShow is show, but in Reader.

rShow :: Show a => ReaderT a Identity String
rShow = undefined

Prelude> runReader rShow 1
"1"
Prelude> fmap (runReader rShow) [1..10]
["1","2","3","4","5","6","7","8","9","10"]

-}

rShow :: Show a => ReaderT a Identity String
rShow       = ReaderT f
  where f x = return $ show x

{-

4. Once you have an rShow that works, make it pointfree.

-}

rShow' :: Show a => ReaderT a Identity String
rShow' = ReaderT $ return . show

{-

5. rPrintAndInc will first print the input with a greeting, then return
the input incremented by one.

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = undefined

Prelude> runReaderT rPrintAndInc 1
Hi: 1
2
Prelude> traverse (runReaderT rPrintAndInc) [1..10]
Hi: 1
Hi: 2
Hi: 3
Hi: 4
Hi: 5
Hi: 6
Hi: 7
Hi: 8
Hi: 9
Hi: 10
[2,3,4,5,6,7,8,9,10,11]

-}

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT f
  where f x  = do
          putStrLn $ "Hi: " <> show x
          return $ x + 1

{-

6. sPrintIncAccum first prints the input with a greeting, then puts
the incremented input as the new state, and returns the original
input as a String.

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = undefined

Prelude> runStateT sPrintIncAccum 10
Hi: 10
("10",11)
Prelude> mapM (runStateT sPrintIncAccum) [1..5]
Hi: 1
Hi: 2
Hi: 3
Hi: 4
Hi: 5
[("1",2),("2",3),("3",4),("4",5),("5",6)]

-}

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT f
  where f x    = do
          putStrLn $ "Hi: " <> show x
          return (show x, x + 1)

{-

Fix the code

The code won’t typecheck as written; fix it so that it does. Feel free to
add imports if it provides something useful. Functions will be used
that we haven’t introduced. You’re not allowed to change the types
asserted. You may have to fix the code in more than one place.

import Control.Monad.Trans.Maybe
import Control.Monad

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e  -> putStrLn ("Good, was very excite: " ++ e)

-}

instance Alternative m => Alternative (MaybeT m) where
  empty :: MaybeT m a
  empty = MaybeT $ pure Nothing

  (<|>) :: MaybeT m a -> MaybeT m a -> MaybeT m a
  (MaybeT m) <|> (MaybeT n) = MaybeT $ m <|> n

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e  -> putStrLn ("Good, was very excite: " ++ e)
