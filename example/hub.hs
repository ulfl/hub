module HubConfig where

import Text.Printf

tags tags xs = map (\(typeStr, x, cmd) -> (typeStr, tags ++ x, cmd)) xs

cmd cmd = [("Command", [], cmd)]

cmds cmd xs = fmap (\x -> ("Command", [x], printf cmd x)) xs

incl file = [("Include", [], file)]

main :: [(String, [String], String)]
main =
    (tags ["bookstore", "us", "production"] $
     bookshop "production.bookshop.com") ++
    (tags ["bookstore", "us", "staging"] $ bookshop "staging.bookshop.com")

bookshop dns =
    (tags ["ssh"] $
     (tags ["application"] $ cmds ("ssh centos@%s." ++ dns) ["app01", "app02"]) ++
     (tags ["database"] $ cmds ("ssh centos@%s." ++ dns) ["db01", "db02"])) ++
    (tags ["ping", "system", "up"] $
          cmd ("curl -v https://" ++ dns ++ "/api/v1/ping")) ++
    (tags ["splunk", "dashboards"] $
          cmd ("echo \"run command to open dashboards\""))
