{-# LANGUAGE TypeFamilies, DatatypeContexts, GADTs, MultiParamTypeClasses, FlexibleInstances #-}

module Spin.SMT where
	import Control.Monad
	import Control.Monad.Trans
	import Control.Monad.Trans.Class
	
	data SM s a = Proceed a | Transition s | Exit s
	data Monad m => SMT s m a = SMT { runSMT :: m (SM s a) }
	
	transition :: Monad m => s -> SMT s m ()
	transition = SMT . return . Transition
	
	exit :: Monad m => s -> SMT s m ()
	exit = SMT . return . Exit
	
	instance Monad m => Monad (SMT s m) where
		(>>=) u f = SMT $ do
			inner <- runSMT u
			case inner of
				Proceed x -> runSMT . f $ x
				Transition s -> return . Transition $ s
				Exit s -> return . Exit $ s
		return x = SMT . return . Proceed $ x
	
	instance MonadTrans (SMT s) where
		lift u = SMT $ u >>= return . Proceed
