{-# LANGUAGE TypeFamilies, DatatypeContexts, GADTs, MultiParamTypeClasses, FlexibleInstances #-}

module Spin.Interfaces where
	import Control.Concurrent
	import Control.Monad
	import Control.Monad.Trans
	{--import Spin.Pipes
	import Spin.SMT--}
	import Text.ParserCombinators.Parsec.Char
	--import Text.Parsec.Prim
	import Text.ParserCombinators.Parsec
	--import Data.Maybe
	
	{--readTemplate :: String -> IO String
	readTemplate template = do
		contents <- readFile $ "templates/" ++ template ++ ".html"
		let html = unlines . init . tail . map (\line -> "s += \"" ++ line ++ "\"") . lines $ contents
		return $ "<script>\nvar s=\"\"\n" ++ html ++ "document.write(str)\n</script>"--}
	
	data Html = Html { getTagName :: String, getAttribs :: [(String, String)], innerHtml :: [Html] } | Markup { getText :: String }
		deriving (Eq, Show)
	data TagType = OpenTag | SingleTag
		deriving (Eq, Show)
	
	attrib :: Parser (String, String)
	attrib = do
		name <- manyTill alphaNum $ do
			many space
			char '='
			many space
			char '\"'
		value <- manyTill anyChar (char '\"')
		return (name, value)
	
	html :: Parser Html
	html = do
		many space
		(tag, attribs, tagType) <- openTag
		case tagType of
			SingleTag -> case tag of
				"m" -> do
					return $ Markup ""
				_ -> do
					return $ Html tag attribs []
			OpenTag -> case tag of
				"m" -> do
					content <- manyTill (try anyChar) . try $ do { endTag tag; many space }
					return $ Markup content
				_ -> do
					content <- manyTill (try html) . try $ do { endTag tag; many space }
					return $ Html tag attribs content
	
	markup :: String -> Parser Html
	markup tag = do
		content <- manyTill anyChar $ endTag tag
		return $ Markup content
	
	openTag :: Parser (String, [(String, String)], TagType)
	openTag = do
		char '<'
		many space
		tag <- many alphaNum
		many space
		attribs <- sepEndBy attrib (many1 space)
		many space
		do { char '/'; many space; char '>'; return (tag, attribs, SingleTag) } <|> do { char '>'; return (tag, attribs, OpenTag) }
	
	endTag :: String -> Parser ()
	endTag tag = do
		char '<'
		many space
		char '/'
		many space
		string tag
		many space
		char '>'
		return ()
