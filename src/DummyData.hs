{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module DummyData
    ( insertInitialProducts
    ) where


import           Control.Monad.Logger (LoggingT)
import           Import


insertInitialProducts :: ReaderT SqlBackend (LoggingT IO) ()
insertInitialProducts = do
  hasPs <- hasProducts
  if not hasPs then insertProducts else return $ ()


hasProducts :: ReaderT SqlBackend (LoggingT IO) Bool
hasProducts = do
  allProducts <- getAllProducts
  return $ length allProducts > 0


insertProducts :: ReaderT SqlBackend (LoggingT IO) ()
insertProducts = do
  _ <- insert $ Product
    "P0001"
    "Identity"
    "Simple functional application. Now free with every order of at least 20 €!"
    False
    0.00
  _ <- insert $ Product
    "P0002" "Maybe" "Encapsulates an optional value" False 1.95
  _ <- insert $ Product
    "P0003" "List" "A finite ordered sequence of values " False 5.50
  _ <- insert $ Product
    "P0004" "IO" "The input/output monad" False 13.02
  _ <- insert $ Product
    "P0005"
    "Reader"
    "Represents a computation, which can read values from a shared environment, pass values from function to function, and execute sub-computations in a modified environment."
    False
    9.99
  _ <- insert $ Product
    "P0006" "ReaderT" "Adds a static environment to a given monad" True 19.99
  _ <- insert $ Product
    "P0007"
    "Writer"
    "Computations which produce a stream of data in addition to the computed values"
    False
    9.99
  _ <- insert $ Product
    "P0008" "WriterT" "Adds Writer capabilities to a given monad" True 9.99
  _ <- insert $ Product
    "P0009" "State" "Lazy state monad" False 9.99
  _ <- insert $ Product
    "P0010" "Error" "Computations which may fail or throw exceptions" False 9.99
  _ <- insert $ Product
    "P0011"
    "Continuation"
    "Computations which can be interrupted and resumed"
    False
    9.99
  return ()


getAllProducts :: DB [Entity Product]
getAllProducts = selectList [] []
