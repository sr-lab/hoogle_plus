module HooglePlus.Example where

type Example = String

parseExample :: String -> Maybe Example
parseExample str = Just str