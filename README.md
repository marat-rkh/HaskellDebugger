# HaskellDebugger

This is the source for a debugger used in [Haskell plugin](https://github.com/Atsky/haskell-idea-plugin) for IntelliJ IDEA.

## Command line arguments

HaskellDebugger accepts specific arguments:
* `-m<file>` - load *file* as main module (required)
* `-p<port>` - connect debug output stream to localhost:*port* (if not set, debug output is set to stdout)
* `-i<path>` - add given *path* as a directory with source files
* `-pkg<package>` - load given *package* as a dependency

## Debugger commands

Commands are mostly the same as that which are used in *ghci*.
* `:?` - show list of available commands
* `:break <mod> <l>` - set a breakpoint for module \<mod\> at the line \<l\>
* `:breakindex <mod> <i>` - set a breakpoint with index \<i\> for module \<mod\>
* `:breaklist <mod>` - show all available breakpoints (index and span) for module \<mod\>
* `:breaklist <mod> <l>` - show all breakpoints containing line \<l\> for module \<mod\> 
* `:continue` - resume after a breakpoint
* `:delete <mod> <ind>` - delete the breakpoint with index \<ind\> from module \<mod\>
* `:force <expr>` - print \<expr\>, forcing unevaluated parts
* `:history` - after *:trace*, show the execution history
* `:sprint <name>` - prints a value without forcing its computation
* `:step` - single-step after stopping at a breakpoint
* `:steplocal` - single-step within the current top-level binding
* `:trace <expr>` - evaluate \<expr\> with tracing on (see *:history*)
* `:type <expr>` - show the type of \<expr\>
* `:eval <int> <expr>` - evaluates \<expr\> ((\<int\> \> 0) =\> forced evaluation)
* `:set <flag>` - sets given flag, available flags: 
    * `-fbreak-on-error`
    * `-fbreak-on-exception`
* `:unset <flag>` - unsets given flag
* `:breakinfo` - pretty printing for current BreakInfo
* `:q` - exit debugger
