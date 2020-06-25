let bookshop =
        λ(dns : Text)
      →   [ web [ "splunk" ] ("https://splunk.intraweb.net?search=" ++ dns) ]
        # cmds
          [ "ssh" ]
          [ "host1", "host2", "host3" ]
          (λ(arg : Text) → "ssh ${arg}." ++ dns)

let config
    : List Command
    = tags
      [ "bookshop" ]
      (   tags [ "staging" ] (bookshop "staging.net")
        # tags [ "production eu" ] (bookshop "production-eu.com")
        # tags [ "production us" ] (bookshop "production-us.com")
      )

in  config
