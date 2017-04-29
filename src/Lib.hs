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
    Nothing -> error "Undeclared variable"


calcIntOp :: IntOp -> [MapValue] -> Int
calcIntOp (Int a) _ = a
calcIntOp (Sym a) store = getVal a store
calcIntOp (Plus  val1 val2) store = calcIntOp val1 store + calcIntOp val2 store
calcIntOp (Minus val1 val2) store = calcIntOp val1 store - calcIntOp val2 store

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

evalItem :: Exp -> [MapValue] -> IO [MapValue]
evalItem (Assign val intOp) store = do
    let res = calcIntOp intOp store
    saveVal (val, res) store

eval :: [Exp] -> [MapValue] -> IO [MapValue]
eval [x] store = evalItem x store
eval (x:xs) store = do
        newStore <- evalItem x store
        eval xs newStore