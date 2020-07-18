let h =
      https://raw.githubusercontent.com/ulfl/hub/master/dhall/hub-prelude.dhall sha256:e95265ddad2a050372e803d519cc916ea3c72247fd6fca7457078f2e96e4f150

let bookshop =
        λ(dns : Text)
      →   [ h.web [ "splunk" ] ("https://splunk.intraweb.net?search=" ++ dns) ]
        # h.cmds
          [ "ssh" ]
          [ "host1", "host2", "host3" ]
          (λ(arg : Text) → "ssh ${arg}." ++ dns)

let config =
      h.tags
      [ "bookshop" ]
      (   h.tags [ "staging" ] (bookshop "staging.net")
        # h.tags [ "production eu" ] (bookshop "production-eu.com")
        # h.tags [ "production us" ] (bookshop "production-us.com")
      )

in  config
