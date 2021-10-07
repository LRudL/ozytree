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

Help will eventually be available in the program itself through the currently-unimplemented `help` command.

## Version history
### Pre-release
#### v0.3.0
2021-10-07

**Features**:

- Added `help` command that provides a list of commands if supplied with no arguments, and help on a specific command if supplied with a command name.
- Changed command parsing error messages.
- Made more arguments in some commands like `make` optional (with default values, enabled by the new command parsing system).

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
