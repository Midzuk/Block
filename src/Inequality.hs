--{-# LANGUAGE TemplateHaskell #-}

module Inequality
  ( 
  ) where
   
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, tryTakeMVar, putMVar, takeMVar)
        
data Stream a = Stream a (MVar (Stream a))

fromList :: [a] -> IO (MVar (Stream a))
fromList [] = newEmptyMVar
fromList (x:xs) = Stream x <$> fromList xs >>= newMVar

toList :: MVar (Stream a) -> IO [a]
toList m = do
  s <- tryTakeMVar m
  case s of
    Nothing -> return []
    Just (Stream x m0) -> (x:) <$> toList m0

  {- do
  s <- fromList xs
  newMVar $ Stream x <$> 
  -}


--大小関係
type Path = ()
type Event = Bool
type Act = Bool -- True: 子, False: 消去

path :: Path -> MVar (Stream Event) -> IO ()
path () m = do
  Stream _ m0 <- takeMVar m
  s <- takeMVar m0
  putMVar m s

inequality :: MVar (Stream Event) -> IO (Maybe Event)
inequality m_ = do
  s_ <- tryTakeMVar m_
  case s_ of
    Nothing -> return Nothing
    Just (Stream e0 m0) -> do
      mo_ <- newEmptyMVar
      forkIO (f e0 m0 mo_) >> takeMVar mo_
  
  where
    f e m mo = do
      s <- tryTakeMVar m
      case s of
        Nothing -> putMVar mo (Just e)
        Just (Stream e1 m1) -> do
          case e1 /=

(~>) :: MVar a -> MVar a -> IO ()
m1 ~> m2 = do
  x <- takeMVar m1
  putMVar m2 x
infix 4 ~>

{-
--大小関係
type Path = Bool
type Event = Maybe Path

inequality :: MVar (Stream Path) -> IO Event
inequality m_ = do
  s <- tryTakeMVar m_
  case s of
    Nothing -> return Nothing
    Just (Stream p m0) -> undefined
  
  where
    f :: Path -> MVar (Stream Path)　-> IO ()
    f p m = do
      s <- tryTakeMVar m
      case s of
        Nothing -> newEmptyMVar >>= putMVar m
        Just (Stream p0 m0) ->
          if p0 == p
            then takeMVar m0 >>= putMVar m
            else Stream p0 <$> f p m0 >>= putMVar m
-}