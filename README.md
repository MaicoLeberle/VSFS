# VSFS
**VSFS** (short for **V**ery **S**imple **F**ile **S**ystem) is intended as a Haskell, command-line-based implementation of some of the usual dynamics present in file system managers. Commands are parsed using [haskeline](https://hackage.haskell.org/package/haskeline). 

Currently, VSFS manages a single virtual file system. I would like to implement the creation and management of multiples file systems in the close future.

\
The VSFS manager accepts the following commands:

+ **pwd** Prints the current directory of the file system.

+ **ls** Lists the content of the current directory (the "(d)" prefix indicates directories).

+ **addFile fileName** dds a file called fileName to the current directory. It prints an error message if the file already exists.

+ **addDir directoryName** Adds a directory called directoryName to the current directory. It prints an error message if the directory already exists.

+ **cd directoryName** Enter the directory called directoryName. It prints an error message if the directory does not exist.

+ **cdup** Changes directory to the immediate parent of the current directory. It prints an error message if the current directory is the root of the file system.

+ **find fileName** Prints the complete path of each occurrence of the fileName under the current directory.

+ **rmFile fileName** Removes the file located at path fileName. It prints an error message if the file cannot be found.

\
Extending the set of commands with user-defined ones should be relatively easy. Adding a new command **cmd** goes as follows:

i) Extend the **Command** data type with a new data constructor (taking as many data types as required in the implementation of **cmd**),

ii) Implement a **cmdMonadic** function whose rightmost type should be **Session ()**, and

iii) Extend the **parseCommand** and **runCommand** functions in Main.hs accordingly.
