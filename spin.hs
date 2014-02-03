{-# LANGUAGE TypeFamilies, DatatypeContexts #-}

module Spin where
	import Network
	import Network.HTTP
	import System.IO
	--import Network.HTTP.Server
	--import Network.HTTP.Server.Response
	import Control.Concurrent
	import Control.Monad
	import Control.Monad.Trans.Class
	import Control.Monad.Trans.State
	
	data Pipe p => Node p = Node { action :: p -> IO NodeStatus }
	data NodeStatus = OK | Quit | Error String deriving (Eq, Show)
	
	class Pipe p where
		type Chunk p :: *
		pull :: p -> IO (Chunk p)
		push :: p -> Chunk p -> IO ()
	
	data HttpOut = HttpOut { hmv :: MVar HttpChunk, hmvo :: MVar HttpChunk }
	data HttpChunk = DataRequest | DataResponse { body :: String } deriving (Eq, Show)
	data Url = Localhost | Url { url :: String }
	
	instance Pipe HttpOut where
		type Chunk HttpOut = HttpChunk
		pull pipe = takeMVar . hmvo $ pipe
		push pipe chunk = putMVar (hmv pipe) chunk
	
	connect site = do
					m <- newEmptyMVar
					n <- newEmptyMVar
					forkIO $ httpThread site m n
					return $ HttpOut m n
	
	data HttpIn = HttpIn { imv :: MVar HttpChunk, omv :: MVar HttpChunk }
	data LocalPort = LocalPort { portof :: PortID }
	
	instance Pipe HttpIn where
		type Chunk HttpIn = HttpChunk
		pull pipe = takeMVar . omv $ pipe
		push pipe chunk = putMVar (imv pipe) chunk
	
	bind site = do
				n <- newEmptyMVar
				o <- newEmptyMVar
				withSocketsDo $ do
									sock <- listenOn . portof $ site
									m <- newEmptyMVar
									forkIO $ sockloop sock m
									forkIO $ sockhandle sock m n o
									return $ HttpIn n o
	
	sockloop sock m = do
						(h, _, _) <- accept sock
						putMVar m h
						sockloop sock m
	
	sockhandle sock m n o = do
							h <- takeMVar m
							putMVar o DataRequest
							chunk <- takeMVar n
							hPutStr h . write_msg . body $ chunk
							hFlush h
							hClose h
							sockhandle sock m n o
	
	data Pipe p => Strict p = Strict { inner :: p, sm :: MVar (Maybe (Chunk p)) }
	
	instance Pipe p => Pipe (Strict p) where
		type Chunk (Strict p) = Maybe (Chunk p)
		pull pipe = do
					val <- takeMVar . sm $ pipe
					putMVar (sm pipe) Nothing
					return val
		push pipe chunk = push (inner pipe) ((\(Just x) -> x) chunk)
	
	stricten pipe = do
					m <- newEmptyMVar
					pipe' <- pipe
					putMVar m Nothing
					forkIO $ strictloop pipe' m
					return $ Strict pipe' m
	
	strictloop pipe m = do
						val <- pull pipe
						takeMVar m -- also handle queues
						putMVar m (Just val)
						strictloop pipe m
	
	{--data Pipe p => Meta p = Meta { mmi :: MVar p, mmo :: MVar p }
	
	instance Pipe p => Pipe (Meta p) where
		type Chunk (Meta p) = p
		pull pipe = takeMVar . mmo $ pipe
		push pipe chunk = putMVar (mmi pipe) chunk--}
	
	data Shuttle a = Shuttle { mmi :: MVar a, mmo :: MVar a}
	
	instance Pipe (Shuttle a) where
		type Chunk (Shuttle a) = a
		pull pipe = takeMVar . mmo $ pipe
		push pipe chunk = putMVar (mmi pipe) chunk
	
	spawn :: (Node (Shuttle a)) -> IO (Shuttle a)
	spawn node = do
		m <- newEmptyMVar
		n <- newEmptyMVar
		let shuttleA = Shuttle m n
		let shuttleB = Shuttle n m
		forkIO $ nhandle node shuttleA
		return shuttleB
	
	nhandle :: Pipe p => Node p -> p -> IO ()
	nhandle node pipe = do
		result <- (action node) pipe
		if (result == OK)
			then do
				nhandle node pipe
			else do
				return ()
	
	httpThread :: Url -> MVar HttpChunk -> MVar HttpChunk -> IO ()
	httpThread url m n = do
							req <- takeMVar m
							if (req == DataRequest)
								then do
									rsp <- (case url of
										Url address -> simpleHTTP (getRequest address)
										Localhost -> simpleHTTP (getRequest "http://localhost"))
									body <- getResponseBody rsp
									putMVar n (DataResponse body)
								else do
									putMVar n (DataResponse "fail: Can only handle request")
							httpThread url m n
	
	write_msg msg = bare_headers ++ "\r\nContent-Length: " ++ (show . length $ msg) ++ "\r\n\r\n" ++ msg ++ "\r\n"
	
	bare_headers = "HTTP/1.1 200 OK\r\nContent-Type: text/html"
	medium_headers = "HTTP/1.1 200 OK\r\nServer: spin.hs/0.0.1 (Ubuntu)\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\nContent-Encoding: gzip"
	full_headers = "HTTP/1.1 200 OK\r\nServer: spin.hs/0.0.1 (Ubuntu)\r\nDate: Sun, 02 Feb 2014 15:52:00 GMT\r\nContent-Type: text/html\r\nLast-Modified: Mon, 06 May 2013 10:26:49 GMT\r\nTransfer-Encoding: chunked\r\nContent-Encoding: gzip"
	
	-- sandbox
	
	-- main routine: every 5 seconds, checks if there are any requests at port 8000. If so, gets data from google.com and pushes it as a response.
	main = do
		pipeServer <- stricten . bind $ LocalPort . PortNumber $ 8000
		pipeOut <- connect $ Url "http://www.google.com"
		forever $ do
			req <- pull pipeServer
			if (req /= Nothing)
				then do
					push pipeOut DataRequest
					res <- pull pipeOut
					push pipeServer $ Just res
				else do
					threadDelay 5000000
