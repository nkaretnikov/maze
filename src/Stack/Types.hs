module Stack.Types where

newtype Stack a = Stack { fromStack :: [a] }
  deriving (Show, Eq)
