module Compiler.AstToClassFileTranslator.Stack (Stack, emptyStack, isEmptyStack, push, pop, top) where

    newtype Stack a = Stack [a] deriving Show
    
    emptyStack = Stack []
    isEmptyStack (Stack xs) = null xs
    push x (Stack xs) = Stack (x:xs)
    pop (Stack (_:xs)) = Stack xs
    top (Stack (x:_)) = x