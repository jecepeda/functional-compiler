module Lib
where

import Grammar

type MapValue = (String, Int)

emptyDataStore :: [MapValue]
emptyDataStore = []

addToDataStore :: [MapValue] -> MapValue -> [MapValue]
addToDataStore l val = val:l

calcIntOp :: IntOp -> Int
calcIntOp (Int a) = a
calcIntOp (Plus a intOp) = a + calcIntOp intOp
calcIntOp (Minus a intOp) = a - calcIntOp intOp

changeMapValue :: MapValue -> MapValue -> MapValue
changeMapValue val1@(a, _) val2@(c, _) 
    | a == c = val1
    | otherwise = val2

updateDataStore :: [MapValue] -> MapValue -> [MapValue]
updateDataStore store mapVal = map (changeMapValue mapVal) store

saveVal :: MapValue-> [MapValue] -> [MapValue]
saveVal (var, val) store = case lookup var store of
    Just _ -> updateDataStore store (var, val)
    Nothing -> addToDataStore store (var, val)

evalItem :: Exp -> [MapValue] -> [MapValue]
evalItem (Assign val intOp) store = saveVal (val, res) store
    where res = calcIntOp intOp
evalItem (Tok intOp) store = do
    let val = calcIntOp(intOp)
    store

eval :: [Exp] -> [MapValue] -> [MapValue]
eval [x] store = evalItem x store
eval (x:xs) store = eval xs (evalItem x store)