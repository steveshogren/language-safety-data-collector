{-# LANGUAGE FlexibleContexts #-}
module Ideas () where

import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell)
import Control.Monad.Reader (Reader,local,ask,MonadReader, runReader)

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

testLocalReaderExample =
  runReader localExample "TEST"
