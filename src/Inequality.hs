--{-# LANGUAGE TemplateHaskell #-}

module Inequality
  ( 
  ) where
   
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, tryTakeMVar, putMVar, takeMVar)
import Control.Monad.Free (Free)
        
data Flow a = Flow a (MVar (Flow a))

type Trigger a = Free MVar a

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


-- act: 資源を1消費
act :: Act -> MVar (Flow Event) -> IO ()
act True m = undefined
act False m = do
  Flow _ m0 <- takeMVar m
  putMVar m m0


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