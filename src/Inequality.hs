--{-# LANGUAGE TemplateHaskell #-}

module Inequality
  ( 
  ) where
   
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, tryTakeMVar)
        
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
type Path = Bool
type Event = Maybe Path

inequality :: MVar (Stream Path) -> IO Event
inequality m_ = do
  s <- tryTakeMVar m_
  case s of
    Nothing -> return Nothing
    Just (Stream p m0) -> undefined
  
  where
    f :: Path -> MVar (Stream Path)　-> IO (MVar (Stream Path))
    f p m = do
      s <- tryTakeMVar m
      case s of
        Nothing -> newEmptyMVar
        Just (Stream p0 m0) ->
          if p0 == p
            then return m0
            else Stream p0 <$> f p m0 >>= newMVar