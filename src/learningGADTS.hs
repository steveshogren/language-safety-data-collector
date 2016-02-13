module LearningGADTS () where

data Expr = I Int
        | B Bool           -- boolean constants
        | Add Expr Expr
        | Mul Expr Expr
        | Eq  Expr Expr    -- equality test

unpack :: Maybe (Either Int Bool) -> Maybe Int
unpack (Just (Left a)) = Just a
unpack _ = Nothing

eval :: Expr -> Maybe (Either Int Bool)
eval (I n) = Just $ Left n
eval (B b) = Just $ Right b
eval (Add e1 e2) = do
  v1 <- unpack . eval $ e1
  v2 <- unpack . eval $ e2
  Just . Left $ v1 + v2
