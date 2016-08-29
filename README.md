# Hub

Hub is a command line tool for keeping track of common shell commands
for yourself and across a development team. These could be commands
that are important but not used frequently enough for you to remember
them or easily find the in your command line history.

Hub allows you to assign tags to each command, and then incrementally
search the command set by entering these tags, while seeing the
filtered set of commands displayed to your terminal.

Commands are defined using standard markdown syntax. Markdown files
can recursively include other markdown files. You could for example
define your own personal commands and then include shared markdown
files for the development team using github repositories. Hub expects
the root markdown file to reside in ```~/.hub.md```.

Nested config files are included as follows:

    ```include
    /absolute/path/to/file.md
    ```

# TODO

* Allow search based on partially entered keywords.

* Improve presentation. Add status bar that lists current number of
  commands.

* Multiline commands (scripts).
