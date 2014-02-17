{-# LANGUAGE TypeFamilies, DatatypeContexts, GADTs #-}

module Spin.Pipes where
	import Control.Concurrent
	import Control.Monad
	import Control.Monad.Trans
	import Spin.SMT
	
	-- "Directional" component for the Pipe GADT
	class Directional d where
		type Invert d :: *
		invert :: Pipe d a -> Pipe (Invert d) a
	
	data Pushable
	data Pullable
	
	instance Directional Pushable where
		type Invert Pushable = Pullable
		invert (Pushable m) = Pullable m
	
	instance Directional Pullable where
		type Invert Pullable = Pushable
		invert (Pullable m) = Pushable m
	
	-- "Pipe" data structure
	data Pipe d a where
		Pushable :: MVar a -> Pipe Pushable a
		Pullable :: MVar a -> Pipe Pullable a
	
	-- "Pipes" Monad
	{--data Monad m => PipesT m a = PipesT { runPipes :: m a }
	
	instance Monad PipesT where
		return = PipesT . return
		(>>=) u f = PipesT $ u >>= (runPipes . f)
	
	extract (Pipes x) = x--}
	
	pull :: Pipe Pullable a -> SMT s IO a
	pull (Pullable m) = lift . takeMVar $ m
	
	push :: Pipe Pushable a -> a -> SMT s IO ()
	push (Pushable m) = lift . putMVar m
	
	construct :: SMT s IO (Pipe Pushable a, Pipe Pullable b)
	construct = do
		ma <- lift newEmptyMVar
		mb <- lift newEmptyMVar
		return (Pushable ma, Pullable mb)
	
	-- "Strict" data structure, similar to pipes but makes nodes non-deterministic
	data Strict a = Strict (MVar (Maybe a))
	
	stricten :: Pipe Pullable a -> IO (Strict a)
	stricten pipe = do
		m <- newEmptyMVar
		n <- newEmptyMVar
		putMVar n Nothing
		forkIO . forever $ do
			(Proceed val) <- runSMT . pull $ pipe
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
