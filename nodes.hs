{-# LANGUAGE TypeFamilies, DatatypeContexts, GADTs #-}

module Nodes where
	import Control.Concurrent
	import Control.Monad
	import Pipes
	
	-- "Evaluation" component for the Node GADT
	class Evaluation a
	
	data Deterministic
	data NonDeterministic
	
	instance Evaluation Deterministic
	instance Evaluation NonDeterministic
	
	-- "Node" data structure
	data Monad m => Node a b s m = Node (Pipe Pullable a -> Pipe Pushable b -> s -> m ())
