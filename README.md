# VSFS
**VSFS** (short for **V**ery **S**imple **F**ile **S**ystem) is intended as a Haskell, command-line-based implementation of some of the usual dynamics present in file system managers. Commands are parsed using [haskeline](https://hackage.haskell.org/package/haskeline). 

VSFS now manages multiple file systems. On the one hand, the file systems **manager** accepts the following commands:

+ **list** prints the list of all the currently running file systems.

+ **init fsName** creates a new file systems called fileName.

+ **switch fsName** either switches to edition mode for file system fsName if it exists, or prints an error message otherwise.

+ **delete fsName** either completely removes the file system called fsName from the manager if it exists, or prints an error message otherwise.  

+ **:q** quites the application.

\
\
On the other hand, after having **switch**ed to a specific file system, the program accepts the following commands:

+ **pwd** Prints the current directory of the file system.

+ **ls** Lists the content of the current directory (the "(d)" prefix indicates directories).

+ **addFile fileName** adds a file called fileName to the current directory. It prints an error message if the file already exists.

+ **addDir directoryName** Adds a directory called directoryName to the current directory. It prints an error message if the directory already exists.

+ **cd directoryName** Enter the directory called directoryName. It prints an error message if the directory does not exist.

+ **cdup** Changes directory to the immediate parent of the current directory. It prints an error message if the current directory is the root of the file system.

+ **find fileName** Prints the complete path of each occurrence of the fileName under the current directory.

+ **rmFile fileName** Removes the file located at path fileName. It prints an error message if the file cannot be found.

+ **:q** switches back to the file systems manager. Note: the current directory is left unchanged. 

\
Extending the set of commands accepted by a file system with user-defined ones should be relatively easy. Adding a new command **cmd** goes as follows:

i) Extend the **Command** data type with a new data constructor (taking as many data types as required in the implementation of **cmd**),

ii) Implement a **cmdMonadic** function whose rightmost type should be **Session ()**, and

iii) Extend the **parseCommand** and **runCommand** functions in Main.hs accordingly.
