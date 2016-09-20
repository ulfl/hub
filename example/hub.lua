function bookshop(dns)
   return append(
      tags("ssh",
           append(tags("application",
                       commands("app01 app02", "ssh centos@%s." .. dns)),
                  tags("database",
                       commands("db01 db02", "ssh centos@%s." .. dns)),
                  tags("log",
                       commands("log01", "ssh centos@%s." .. dns)))),
      command("up", "curl https://" .. dns .. "/api/v1/up"))
end

cmds = append(tags("staging", bookshop("staging.bookshop.com")),
              tags("production", bookshop("production.bookshop.com")))

return(cmds)
