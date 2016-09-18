-- Copyright (C) 2016 Ulf Leopold
--
module Hub.CommandType
  ( Command(Command)
  ) where

type Tags = [String]
data Command = Command Tags String deriving (Ord, Show, Eq)
