# Hub

Hub is a command line tool for keeping track of common shell commands
for yourself and across a development team. These could be commands
that are important but not used frequently enough for you to remember
them or easily find the in your command line history.

Hub allows you to assign tags to each command, and then incrementally
search the command set by entering these tags, while seeing the
filtered set of commands displayed to your terminal.

Commands are defined in configuration files. These can either be in
Lua or markdown format. Lua provides the greatest flixibility and
allows for a large number of similar commands to be specified while
keeping the configuration file DRY.

Both Lua and markdown allows for recursive inclusion of other Hub
config files. You can for example define your own personal commands in
the root configuration file and then include shared configuration
files for the development team (for example stored in git
repositories). Hub expects the root configuration file to reside in
`~/.hub.lua` or `~/.hub.md`.

Nested config files are included as follows in Lua:

    dofile "/absolute/path/to/file/file.lua"

and as follows in markdown files:

    ```include
    /absolute/path/to/file.md
    ```
For an example config, see [hub.lua](./example/hub.lua)

# Installation

## OSX

```bash
brew tap ulfl/tap
brew install ulfl/tap/hub
```


## Ubuntu/Debian

Copy the prebuilt `binaries/hub-ubuntu` into your path and rename it to `hub`.


# TODO

* Remember popular commands and sort accordingly. Should be easy to
  rerun a command that was just executed.

* Multiline commands (scripts).
