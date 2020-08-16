# Description

Hub is a command line tool for keeping track of common shell commands and web
links for yourself and across a development team. These could be commands that
are important but not used frequently enough for you to remember them or easily
find them in your command line history. If the Hub configuration file is stored
in a shared git repository, then a development team can work together to ensure
that all commands are kept up to date.

Hub allows you to assign tags to each command, and then incrementally search the
command set by entering these tags, while seeing the filtered set of commands
displayed to your terminal. Hub can also interface to Emacs Ivy mode and execute
commands via Eshell.

Commands are defined in configuration files expressed in the Dhall language. The
configuration format allows for a large number of similar commands to be
specified while keeping the configuration file DRY. [Dhall also provides safety
guarantees](https://docs.dhall-lang.org/discussions/Safety-guarantees.html) so
loading a shared configuration file can not have negative side effects.

Dhall allows for recursive inclusion of other Hub config files. You can for
example define your own personal commands in the root configuration file and
then include shared configuration files for the development team. Hub expects
the root configuration file to reside in `~/.hub.dhall`.

For an example config, see [hub.dhall](./example/hub.dhall)
