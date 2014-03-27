{-# LANGUAGE TypeFamilies, DatatypeContexts, GADTs, MultiParamTypeClasses, FlexibleInstances #-}

module Spin.Nodes where
	import Control.Concurrent
	import Control.Monad
	import Control.Monad.Trans
	import System.IO.Unsafe
	import Spin.Pipes
	import Spin.SMT
	
	data Node pin pout s =	Node s ((Pipe Pushable pout, Pipe Pullable pin) -> s -> SMT s IO ())
	
	--start :: Node pin pout s -> (pout, pin) -> SMT s IO ()
	start ps (Node s f) = lift $ loopNode (Node s f) ps s >> return ()
	
	loopNode node ps state = do
		result <- runNode node ps state
		case result of
			Transition newstate -> loopNode node ps newstate
			Exit exitstate -> return ()
			Proceed x -> fail "State machine must either exit or transition to a new state"
	
	runNode (Node s n) ps state = runSMT $ n ps state
	
	spawn node = do
		(ppush, ppull) <- construct
		lift . forkIO $ do
			runSMT . start (ppush, ppull) $ node
			return ()
		return (invert ppush, invert ppull)
	
	spawnWith (ppush, ppull) node = do
		lift . forkIO $ do
			runSMT . start (ppush, ppull) $ node
			return ()
		return (invert ppush, invert ppull)
