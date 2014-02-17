module Main where
	import Spin.Nodes
	import Spin.Pipes
	import Spin.SMT
	import Control.Monad
	import Control.Monad.Trans
	
	data NState = Starting | Running | Stopping | Status Int
	
	main = runSMT $ do
		(pout, pin) <- spawn $ Pure Starting node2
		start (Pure Starting node1) (pin, pout)
		return ()
	
	node1 (pout, pin) state = case state of
			Starting -> do
				lift . putStrLn $ "Starting node1..."
				transition Running
			Running -> do	
				push pout "Run."
				msg <- pull pin
				lift . putStrLn $ msg
				transition Stopping
			Stopping -> do
				exit $ Status 0
	node2 (pout, pin) state = case state of
			Starting -> do
				lift . putStrLn $ "Starting node2..."
				sig <- pull pin
				transition Running
			Running -> do
				push pout "Hello World!"
				transition Stopping
			Stopping -> do
				exit $ Status 0
