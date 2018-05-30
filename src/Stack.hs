module Stack where

import Stack.Types

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty _          = False

push :: Stack a -> a -> Stack a
push (Stack xs) x = Stack (x : xs)

-- XXX: Potential runtime error here can be eliminated with 'NonEmpty'.
-- But I'd like to stick to the original maze algorithm for now.
pop :: Stack a -> (a, Stack a)
pop (Stack [])     = error "pop: empty stack"
pop (Stack (x:xs)) = (x, Stack xs)
