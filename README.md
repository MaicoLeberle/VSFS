# VSFS
**VSFS** (short for **V**ery **S**imple **F**ile **S**ystem) is intended as a Haskell, command-line-based implementation of some of the usual dynamics present in file system managers. Commands are parsed using [haskeline](https://hackage.haskell.org/package/haskeline). 

VSFS now manages multiple file systems. On the one hand, the file systems **manager** accepts the following commands:

+ **list** Lists all the file systems.

+ **init fs** Initializes a file system called fs.

+ **switch fs** Switches to file system manager for fs.

+ **delete fs** Deletes file system fs.  

+ **:q** Quits.

\
\
On the other hand, after having **switch**ed to a specific file system, the program accepts the following commands:

+ **pwd** Prints the current directory of the file system.

+ **ls** Lists the content of the current directory (the "(d)" prefix indicates directories).

+ **mkDir d** Adds directory d within the current directory.

+ **cd d** Enters directory d located in the current directory.

+ **cdup** Changes directory to the immediate parent of the current directory.

+ **addFile f** Adds file f to the current directory.

+ **rmFile f** Removes file located at path p, starting from the current directory.

+ **find f** Prints the complete path of each occurrence of f under the current directory.

+ **:q** switches back to the file systems manager. Note: the current directory is left unchanged. 

\
Extending the set of commands accepted by a file system with user-defined ones should be relatively easy. Adding a new command **cmd** goes as follows:

i) Extend data type **Command**, in src/Types.hs, with a new data constructor, making sure to include as many data types as information required in the implementation of **cmd**,

ii) Implement pure function **cmd**, which updates the state of the file system according to the intended logic of the command,

iii) Implement a **cmdMonadic** function whose return type should be **Session ()**, and

iv) Extend the **parseCommand** and **runCommand** functions in Main.hs accordingly.
