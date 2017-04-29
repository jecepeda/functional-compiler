module Lib
where

import Grammar

type MapValue = (String, Int)

emptyDataStore :: [MapValue]
emptyDataStore = []

addToDataStore :: [MapValue] -> MapValue -> [MapValue]
addToDataStore l val = val:l

getVal :: String -> [MapValue] -> Int
getVal a store = case lookup a store of
    Just val -> val
    Nothing  -> 0

calcIntOp :: IntOp -> [MapValue] -> Int
calcIntOp (Int a) _ = a
calcIntOp (Sym a) store = getVal a store
calcIntOp (Plus  val1 val2) store = calcIntOp val1 store + calcIntOp val2 store
calcIntOp (Minus val1 val2) store = calcIntOp val1 store - calcIntOp val2 store
calcIntOp (Multiply val1 val2) store = calcIntOp val1 store * calcIntOp val2 store
calcIntOp (Divide val1 val2) store = div (calcIntOp val1 store) (calcIntOp val2 store)

changeMapValue :: MapValue -> MapValue -> MapValue
changeMapValue val1@(a, _) val2@(c, _) 
    | a == c = val1
    | otherwise = val2

updateDataStore :: [MapValue] -> MapValue -> [MapValue]
updateDataStore store mapVal = map (changeMapValue mapVal) store

saveVal :: MapValue-> [MapValue] -> IO [MapValue]
saveVal (var, val) store = return ( case lookup var store of
    Just _ -> updateDataStore store (var, val)
    Nothing -> addToDataStore store (var, val))

evalCond :: Conditional -> [MapValue] -> Bool
evalCond (Less val1 val2) store = calcIntOp val1 store < calcIntOp val2 store
evalCond (LessEqual val1 val2) store = calcIntOp val1 store <= calcIntOp val2 store
evalCond (Greater val1 val2) store = calcIntOp val1 store > calcIntOp val2 store
evalCond (GreaterEqual val1 val2) store = calcIntOp val1 store >= calcIntOp val2 store 


evalIfExpression :: IfBody -> [MapValue] -> IO [MapValue]
evalIfExpression (If cond part1) store = do
    if evalCond cond store
        then eval part1 store
        else return store
evalIfExpression (IfElse cond part1 part2) store = do
    if evalCond cond store
        then eval part1 store
        else eval part2 store

evalWhileExpression :: Conditional -> [Exp] -> [MapValue] -> IO [MapValue]
evalWhileExpression cond val store = do
    if evalCond cond store
        then do
            newStore <- eval val store
            evalWhileExpression cond val newStore
        else return store

printValue :: IntOp -> [MapValue] -> IO [MapValue]
printValue val store = do
    print (calcIntOp val store)
    return store

evalItem :: Exp -> [MapValue] -> IO [MapValue]
evalItem (Assign val intOp) store = do
    let res = calcIntOp intOp store
    saveVal (val, res) store
evalItem (Tok val) store = do
    let _ = calcIntOp val store
    return store
evalItem (IfExp val) store = do
    evalIfExpression val store
evalItem (Print val) store = do
    printValue val store
evalItem (WhileExp cond val) store = do
    evalWhileExpression cond val store

eval :: [Exp] -> [MapValue] -> IO [MapValue]
eval [x] store = evalItem x store
eval (x:xs) store = do
        newStore <- evalItem x store
        eval xs newStore