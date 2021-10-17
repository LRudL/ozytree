# ozytree

> And on the pedestal these words appear:
> 
> "My name is Ozymandias, king of kings;
> 
> Look on my works, ye Mighty, and despair!"
> 
> Nothing beside remains. Round the decay
> 
> Of that colossal wreck, boundless and bare
> 
> The lone and level sands stretch far away.

\- Percy Shelley, "Ozymandias"

With the productive power of Ozytree, you too can become a king of kings who first achieves great things and then inevitably becomes a symbol of the transcience of all achievement. Ozytree helps you do this with five key features:

- It lets you keep track of your tasks.
- Unlike all those peasant to-do-list programs, Ozytree acknowledges that your tasks naturally form a nested tree structure. It is a to-do-**tree** program.
- Ozytree foregoes all that hippy GUI nonsense, instead using a fast, productive command line interface.
- Ozytree maintains a record of every state your task tree has ever been in. You can manage history in a Git-style fashion, and browse through snapshots of your task tree in time.
- Ozytree does absolutely nothing to forestall the inevitable death and decay of you and everything you do.

## Usage

You need [Racket](https://racket-lang.org/).

Once you have Racket, and have downloaded the Ozytree source folder, navigate into it and run `racket main.rkt`. If it is your first time starting Ozytree, you will be guided through the configuration process.

Help is available in the program itself through the `help` command.

## Version history
### Planned features
- Setting deadlines
- Sort option by deadline and by size divided by time to deadline.
- Setting to hide/unhide completed.
- Jumping to a specific state in the history viewer.

### Pre-release
#### v0.5.2
2021-10-17

**Bug fixes**:
- Fixed a series of complicated bugs relating to ID generation for tasks and its interaction with commits, `history-mode` (and the rollback feature of `history-mode`), and restoring tree state from the history file.

**Internal**:
- `history.txt` now includes task IDs next to lines for `make` commands, both for human readability and to ensure compatibility with future versions without breaking changes (previous system meantIDs were only implicit, so any change to ID generation semantics would be potentially breaking between versions).

#### v0.5.1
2021-10-11

**Bug fixes**:
- Commits broken because of function missing in the history functions table.

#### v0.5.0
2021-10-11

**Features**:
- Marking/unmarking a node (i.e. saying it is complete/incomplete) now automatically applies to all descendant nodes of the node.
- History viewer: the `history-mode` command will take you into a special mode where you can navigate forwards and backwards in time, and also rollback to a previous state in history.

**Bug fixes**:
- Crashes when interpreting a command are now caught and the command loop continues, rather than Ozytree itself crashing completely.

**Internal**:
- Refactored command loop code.
- Refactored the dependency structure of the history-related modules, especially as they relate to command interpretation.

#### v0.4.0
2021-10-10

**Features**:
- Coloured text to make the tree easier to read, some other ANSI escape code -related niceness.
- Sorting options: the `sort` command, with options for sorting by the task size (including children), by task id, and for inverting the sort order.
- Saving of sorting settings.
- *BREAKING CHANGE:* The `set` command has been replaced with `set-size` and `set-text`.

**Bug fixes**:
- The `undo` command removes commands from the list in the right order (rather than the reverse order: least recently added first).

**Internal**:
- Refactoring of the `create-command` macro to enable adding argumentless commands more easily.

#### v0.3.0
2021-10-07

**Features**:

- Added `help` command
- Changed command parsing error messages.
- Made more arguments in some commands like `make` optional (with default values, enabled by the new command parsing system).

**Bug fixes**:
- Entering a blank line as a command no longer crashes the program.

**Internal**:
- Complete rewrite of command parsing system.
- Refactor of command defining code (now with a macro that does all the work, need to change only one place to define new commands).
- Refactor of the command loop in `main.rkt` and of code in `cli.rkt` to integrate with the new command parsing and defining system.8

#### v0.2.0
2021-10-02

**Features**:

- Total size of task tree rooted at each node now displayed in print-out.
- Nicer and more informative logging messages for `undo`, `commit`, and on startup.

**Internal**:
- Refactored maximum node id finder with a new general tree reducer.
- Deleted `printing.rkt`, functionality moved into `tree.rkt`.

#### v0.1.0
- Initial
