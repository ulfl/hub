function bookshop(dns)
   return
      tags("",
           tags("ssh",
                tags("application",
                     commands("app01 app02", "ssh centos@%s." .. dns)),
                tags("database",
                     commands("db01 db02", "ssh centos@%s." .. dns)),
                tags("log",
                     commands("log01", "ssh centos@%s." .. dns))),
           command("up", "curl https://" .. dns .. "/api/v1/up"),
           web("splunk", "https://splunk.intraweb." .. dns)
      )
end

cmds = tags("bookshop",
            tags("staging", bookshop("staging.bookshop3425987.com")),
            tags("us usa production", bookshop("production.us.bookshop3425987.com")),
            tags("eu europe production", bookshop("production.eu.bookshop3425987.com")))

return(cmds)
