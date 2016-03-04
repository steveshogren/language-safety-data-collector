{-# LANGUAGE FlexibleContexts #-}
module Ideas () where

import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell)
import Control.Monad.Reader (Reader,local,ask,MonadReader, runReader, ReaderT, runReaderT)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import Control.Monad.State (StateT, get, put, runStateT)

-- The Reader monad allows us to simply pass around some state to ask for
-- or adjust the state using local
-- Super easy way to pass a DB connection string perhaps...
myName :: (MonadReader String m) => String -> m String
myName step = do
  name <- ask
  return (step ++ ", I am " ++ name)

localExample :: Reader String (String, String, String)
localExample = do
  a <- myName "First"
  b <- local (++"dy") (myName "Second")
  c <- myName "Third"
  return (a, b, c)

readerExample =
  runReader localExample "TEST"

-- combining Reader and IO by putting ReaderT as the "top-wrapper"
-- to do this, we are "in Reader" at the top level, and need to liftIO
-- our IO functions up into ReaderT
listDir :: ReaderT String IO [FilePath]
listDir = do
  name <- ask
  conts <- liftIO $ getDirectoryContents "."
  return $ map ((++) name) conts

readerTExample =
  runReaderT listDir "ETSET"

-- state appears to be more like a mutable pointer, changing it
-- with a put changes the value of the bound symbol
listDirS :: StateT String IO ([FilePath], String)
listDirS = do
  conts <- liftIO $ getDirectoryContents "."
  state <- get
  put (state++(concat conts))
  return (conts, state)

stateTExample =
  runStateT listDirS "ETSET"
