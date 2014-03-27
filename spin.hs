{-# LANGUAGE TypeFamilies, DatatypeContexts #-}

module Spin where
	import Network
	import Network.HTTP
	import System.IO
	import Control.Concurrent
	import Control.Monad
	import Control.Monad.Trans.Class
	import Control.Monad.Trans.State
	import Spin.Pipes
	import Spin.Nodes
	import Spin.SMT
	import Spin.Interfaces
	
	data DataRequest = DataRequest String | DataClose
	data DataResponse a = DataResponse { getBody :: a }
	
	data GenericNodeState a b = Starting a | Running b | Stopping b | Stopped Int
	
	connect :: String -> SMT s IO (Pipe Pullable (DataResponse String), Pipe Pushable DataRequest)
	connect host = spawn $ Node (Starting host) httpNode
	
	httpNode (pout, pin) state = case state of
		Starting host -> do
			transition (Running host)
		Running host -> do
			req <- pull pin
			case req of
				DataRequest path -> do
					rsp <- lift . simpleHTTP $ getRequest (host ++ path)
					body <- lift $ getResponseBody rsp
					push pout (DataResponse body)
					transition (Running host)
				DataClose -> do
					exit (Stopped 0)
	
	bind :: PortNumber -> SMT s IO (Pipe Pullable (Pipe Pullable DataRequest, Pipe Pushable (DataResponse String)))
	bind site = do
		ps <- spawn $ Node (Starting ((PortNumber site), Nothing)) bindNode
		return $ fst ps
	
	bindNode (pout, pin) state = case state of
		Starting (site, _) -> do
			sock <- lift . listenOn $ site
			transition (Running (site, Just sock))
		Running (site, Just sock) -> do
			(h, hostname, portnumber) <- lift . accept $ sock
			ps <- spawn $ Node (Starting h) portHandlerNode
			push pout ps
			transition (Running (site, Just sock))
	
	portHandlerNode (pout, pin) state = case state of
		Starting h -> do
			d <- lift . hGetLine $ h
			let url = takeWhile (/= ' ') . drop 4 $ d
			push pout $ DataRequest url
			transition (Running h)
		Running h -> do
			resp <- pull pin
			lift . hPutStr h . write_msg . getBody $ resp
			transition (Stopping h)
		Stopping h -> do
			lift . hFlush $ h
			lift . hClose $ h
			exit (Stopped 0)
	
	write_msg msg = bare_headers ++ "\r\nContent-Length: " ++ (show . length $ msg) ++ "\r\n\r\n" ++ msg ++ "\r\n"
	
	keep_alive_headers = "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nConnection: Keep-Alive"
	bare_headers = "HTTP/1.1 200 OK\r\nContent-Type: text/html"
	medium_headers = "HTTP/1.1 200 OK\r\nServer: spin.hs/0.0.1 (Ubuntu)\r\nContent-Type: text/html\r\nTransfer-Encoding: chunked\r\nContent-Encoding: gzip"
	full_headers = "HTTP/1.1 200 OK\r\nServer: spin.hs/0.0.1 (Ubuntu)\r\nDate: Sun, 02 Feb 2014 15:52:00 GMT\r\nContent-Type: text/html\r\nLast-Modified: Mon, 06 May 2013 10:26:49 GMT\r\nTransfer-Encoding: chunked\r\nContent-Encoding: gzip"
	
	-- sandbox
	
	main = runSMT $ do
		ps <- construct
		start ps (Node (Starting ()) nodeMain)
		return ()
	
	-- when a request arrives at port 8000, pulls data from http://www.google.com and pushes it as the response
	nodeMain (pin, pout) state = case state of
		Starting _ -> do
			p8000 <- bind 8000
			(pgoogle_in, pgoogle_out) <- connect "http://www.google.com"
			transition $ Running (p8000, pgoogle_in, pgoogle_out)
		Running (p8000, pgoogle_in, pgoogle_out) -> do
			(ppull, ppush) <- pull p8000
			DataRequest path <- pull ppull
			case path of
				"/localhost/index.html" -> do
					response <- lift . readTemplate $ "main"
					push ppush $ DataResponse response
				_ -> do
					push pgoogle_out $ DataRequest path
					response <- pull pgoogle_in
					push ppush $ response
			transition (Running (p8000, pgoogle_in, pgoogle_out))
