{-# LANGUAGE TypeFamilies, DatatypeContexts, GADTs #-}

module Pipes where
	import Control.Concurrent
	import Control.Monad
	
	-- "Directional" component for the Pipe GADT
	class Directional a
	
	data Pushable
	data Pullable
	
	instance Directional Pushable
	instance Directional Pullable
	
	-- "Pipe" data structure
	data Pipe d a where
		Pushable :: MVar a -> Pipe Pushable a
		Pullable :: MVar a -> Pipe Pullable a
	
	pull :: Pipe Pullable a -> IO a
	pull (Pullable m) = takeMVar m
	
	push :: Pipe Pushable a -> a -> IO ()
	push (Pushable m) x = putMVar m x
	
	-- "Strict" data structure, similar to pipes but makes nodes non-deterministic
	data Strict a = Strict (MVar (Maybe a))
	
	stricten :: Pipe Pullable a -> IO (Strict a)
	stricten (Pullable o) = do
		m <- newEmptyMVar
		n <- newEmptyMVar
		putMVar n Nothing
		forkIO . forever $ do
			val <- takeMVar o
			takeMVar n
			putMVar n (Just val)
		forkIO . forever $ do
			val <- takeMVar n
			putMVar n Nothing
			putMVar m val
		return $ Strict m
	
	-- TODO: replace Strict with Pipe + Non-deterministic Node + Pipe?
	check :: Strict a -> IO (Maybe a)
	check (Strict m) = takeMVar m
	
	unstricten :: Strict a -> IO (Pipe Pullable a)
	unstricten = undefined
