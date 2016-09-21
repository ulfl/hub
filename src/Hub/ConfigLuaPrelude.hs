-- Copyright (C) 2016 Ulf Leopold
--
{-# LANGUAGE QuasiQuotes #-}

module Hub.ConfigLuaPrelude (hubLuaPrelude) where

import Data.String.Here

hubLuaPrelude = [here|
function tags(tags, cmds)
   tags = handleTagsSpec(tags)
   return(map(function (cmd) return prependTags(tags, cmd) end, cmds))
end

function commands(tags, cmdFormat)
   return map(function (x) return {{x}, string.format(cmdFormat, x)} end,
      handleTagsSpec(tags))
end

function command(tags, cmd)
   return {{handleTagsSpec(tags), cmd}}
end

function handleTagsSpec(tags)
   if type(tags) == 'table' then
      return tags
   elseif type(tags) == 'string' then
      return split(tags)
   end
end

function split(str)
   words = {}
   for word in str:gmatch("[%w-_]+") do table.insert(words, word) end
   return words
end

function map(func, array)
   local newArray = {}
   for i,v in ipairs(array) do
      newArray[i] = func(v)
   end
   return newArray
end

function prependTags(tags, cmd)
   local newTags = {}

   for _, v in ipairs(tags) do
      table.insert(newTags, v)
   end

   for _, v in ipairs(cmd[1]) do
      table.insert(newTags, v)
   end

   return {newTags, cmd[2]}
end

function deepcopy(orig)
    local orig_type = type(orig)
    local copy
    if orig_type == 'table' then
        copy = {}
        for orig_key, orig_value in next, orig, nil do
            copy[deepcopy(orig_key)] = deepcopy(orig_value)
        end
        setmetatable(copy, deepcopy(getmetatable(orig)))
    else -- number, string, boolean, etc
        copy = orig
    end
    return copy
end

function append(cmds, ...)
   for i,v in ipairs(arg) do
      for _, w in pairs(v) do
         table.insert(cmds, w)
      end
   end
   return cmds
end
|]
