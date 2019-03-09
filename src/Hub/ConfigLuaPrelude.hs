-- Copyright (C) 2016-2019 Ulf Leopold
--
module Hub.ConfigLuaPrelude (hubLuaPrelude) where

import Data.String.Here

hubLuaPrelude :: String
hubLuaPrelude = [here|
-------------------------------------------------------------------------------
-- Extensions to the stdlib

function table.map(t, f)
   local rv = {}
   for i, v in ipairs(t) do
      rv[i] = f(v)
   end
   return rv
end

function table.extend(t, o)
   for _, v in pairs(o) do
      table.insert(t, v)
   end
end

function table.concat(...)
   local rv = {}
   for _, o in pairs({...}) do
      table.extend(rv, o)
   end
   return rv
end

function string.trim(s)
  return s:gsub("^%s*(.-)%s*$", "%1")
end

function string.split(s)
   local rv = {}
   for v in s:gmatch("[%w-_]+") do
      table.insert(rv, v)
   end
   return rv
end

function os.capture(cmd)
  local f = assert(io.popen(cmd, 'r'))
  local s = assert(f:read('*a'))
  f:close()
  return s
end

-------------------------------------------------------------------------------
-- Public API

function command(tags, cmd)
   tags = _tags(tags)
   return { _make(tags, cmd) }
end

function commands(tags, fmt)
   tags = _tags(tags)
   return table.map(tags, function (t)
       return _make({ t }, fmt:format(t))
   end)
end

function web(tags, cmd)
   tags = _tags(tags)
   return { _make(tags, open_cmd .. " " .. cmd) }
end

function tags(tags, ...)
   tags = _tags(tags)
   return table.map(table.concat(...), function (cmd)
     return { table.concat(tags, cmd[1]), cmd[2] }
   end)
end

-------------------------------------------------------------------------------
-- Helper functions

_open_cmd = ""
if os.capture("uname"):trim() == "Darwin" then
   open_cmd = "open"
else
   open_cmd = "xdg-open"
end

function _make(tags, cmd)
    return { tags, cmd }
end

function _tags(tags)
   if type(tags) == 'table' then
      return tags
   elseif type(tags) == 'string' then
      return tags:split()
   end
end
|]
