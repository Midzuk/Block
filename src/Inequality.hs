--{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}


module Inequality
  ( 
  ) where
   
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, tryTakeMVar, putMVar, takeMVar)
import Control.Monad.Free (Free(..))
import Control.Comonad.Cofree (Cofree(..))
import Control.Comonad.Trans.Cofree (CofreeT(..))
import Control.Monad 
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Applicative
import Data.Function (on)
        
--data Flow a = Flow a (MVar (Flow a))

--type Trigger a = Free MVar a
--data Flow a = Flow a (MVar (Flow a))
--type FreeFlow a = Free Flow a

-- type Flow a = [MVar (Trigger a)]
-- type Trigger a = Free Flow a

-- data Flow a = Flow (Free Flow a) (MVar (Flow a))

{-
newTrigger :: a -> IO (Trigger a)
newTrigger x = Free <$> newMVar (Pure x)

newEmptyTrigger :: IO (Trigger a)
newEmptyTrigger = Free <$> newEmptyMVar
-}

newtype Object a = Object { runObject :: IO (MVar a) }

--consume :: Object a -> IO a
--consume x = runObject x >>= takeMVar

instance Functor Object where
  fmap f x = Object $ (f <$> (runObject >=> takeMVar) x) >>= newMVar

instance Applicative Object where
  pure = Object . newMVar
  f <*> x = Object $ ((runObject >=> takeMVar) f <*> (runObject >=> takeMVar) x) >>= newMVar

instance Monad Object where
  x >>= f = Object $ ((runObject >=> takeMVar) x >>= ((runObject >=> takeMVar) . f)) >>= newMVar

instance Alternative Object where
  empty = Object newEmptyMVar
  l <|> r = Object $ ((<|>) `on` (runObject >=> takeMVar)) l r >>= newMVar

  {-do
    l <- (runObject >=> tryTakeMVar) l_
    r <- (runObject >=> tryTakeMVar) r_
    case l <|> r of
-}


type Flow a = Cofree Object a
type Trigger a = Free Object a

--data Path a = Flow (Cofree Object (Path a)) | Trigger (Free Object (Path a))

--newtype Path a = Path (CofreeT (Free Object) a)


{-
pull :: Trigger a -> IO a
pull (Pure x) = return x
pull (Free m) = takeMVar m >>= pull

tryPull :: Trigger a -> IO (Maybe a)
tryPull (Pure x) = return $ Just x
tryPull (Free m) = runMaybeT $ MaybeT (tryTakeMVar m) >>= (MaybeT . tryPull)
--tryPull (Free m) = tryTakeMVar m >>= ((join <$>) . sequence . (tryPull <$>))

peel :: Trigger a -> IO (Trigger a)
peel (Free m) = takeMVar m

tryPeel :: Trigger a -> IO (Maybe (Trigger a))
tryPeel (Free m) = tryTakeMVar m



makeTriggerList :: [a] -> IO [Trigger a]
makeTriggerList = traverse newTrigger

type Act = Bool

inequality :: [Trigger Act] -> IO (Maybe Act)
inequality ts = do
  mo <- newEmptyMVar
  mmo <- newMVar (Free mo)

  --forkIO $ putMVar (head ts) (Free mmo)



  t <- takeMVar mmo
  tryPull t


-}

{-
fromList :: [a] -> IO (MVar (Flow a))
fromList [] = newEmptyMVar
fromList (x:xs) = Flow x <$> fromList xs >>= newMVar

toList :: MVar (Flow a) -> IO [a]
toList m = do
  s <- tryTakeMVar m
  case s of
    Nothing -> return []
    Just (Flow x m0) -> (x:) <$> toList m0

  {- do
  s <- fromList xs
  newMVar $ Flow x <$> 
  -}


--大小関係
type Path = ()
type Act = Bool -- True: 子, False: 消去
type Event = Act

path :: Path -> MVar (Flow Event) -> IO ()
path () m = do
  Flow _ m0 <- takeMVar m
  s <- takeMVar m0
  putMVar m s

-}

{-
-- act: 資源を1消費
act :: Act -> MVar (Flow Event) -> IO ()
act True m = undefined
act False m = do
  Flow _ m0 <- takeMVar m
  putMVar m m0
-}

{-
inequality :: MVar (Flow Event) -> IO (Maybe Event)
inequality m_ = do
  s_ <- tryTakeMVar m_
  case s_ of
    Nothing -> return Nothing
    Just (Flow e0 m0) -> do
      mo_ <- newEmptyMVar
      forkIO (f e0 m0 mo_) >> takeMVar mo_
  
  where
    f e m mo = do
      s <- tryTakeMVar m
      case s of
        Nothing -> putMVar mo (Just e)
        Just (Flow e1 m1) -> undefined

(~>) :: MVar a -> MVar a -> IO ()
m1 ~> m2 = do
  x <- takeMVar m1
  putMVar m2 x
infix 4 ~>

-}
{-
--大小関係
type Path = Bool
type Event = Maybe Path

inequality :: MVar (Flow Path) -> IO Event
inequality m_ = do
  s <- tryTakeMVar m_
  case s of
    Nothing -> return Nothing
    Just (Flow p m0) -> undefined
  
  where
    f :: Path -> MVar (Flow Path)　-> IO ()
    f p m = do
      s <- tryTakeMVar m
      case s of
        Nothing -> newEmptyMVar >>= putMVar m
        Just (Flow p0 m0) ->
          if p0 == p
            then takeMVar m0 >>= putMVar m
            else Flow p0 <$> f p m0 >>= putMVar m
-}