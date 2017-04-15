module Lib
where

import Grammar

type MapValue = (String, Integer)
type DataStore = [MapValue]

emptyDataStore :: DataStore
emptyDataStore = []

addToDataStore :: DataStore -> MapValue -> DataStore
addToDataStore l val = l ++ [val]

-- TODO: add logic to support grammar --
evalItem :: Exp -> DataStore -> DataStore
evalItem _ store = store

eval :: [Exp] -> DataStore -> DataStore
eval [x] store = evalItem x store
eval (x:xs) store = eval xs (evalItem x store)