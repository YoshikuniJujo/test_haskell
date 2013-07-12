{-# LANGUAGE PackageImports #-}

import ReadDirectory

import "monads-tf" Control.Monad.State
import System.IO

type FileSystemM = StateT FSZipper IO

ls :: FileSystemM ()
ls = do	fs <- get
	case fsGetDirContents fs of
		Just cs -> lift $ mapM_ putStrLn cs
		_ -> return ()

cd :: Name -> FileSystemM ()
cd ".." = do
	fs <- get
	case fsUp fs of
		Just nfs -> put nfs
		_ -> return ()
cd fp = do
	fs <- get
	case fsTo fp fs of
		Just nfs -> put nfs
		_ -> return ()

cat :: FileSystemM ()
cat = do
	fs <- get
	case fsGetContents fs of
		Just cs -> lift $ putStr cs
		_ -> return ()

shell :: FileSystemM ()
shell = do
	command <- lift $ do
		putStr "> "
		hFlush stdout
		getLine
	case command of
		"ls" -> ls >> shell
		('c' : 'd' : ' ' : n) -> cd n >> shell
		"cat" -> cat >> shell
		"exit" -> return ()
		_ -> shell
