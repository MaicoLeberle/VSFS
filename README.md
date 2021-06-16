# VSFS
**VSFS** (short for **V**ery **S**imple **F**ile **S**/ystem) is intended as a Haskell, command-line-based implementation of some of the usual dynamics present in file system managers.

\
File systems are implemented as a pair **(d, t) ::(Directory, Trail)**, where
+ **d :: Directory** is the current directory of the file system, and

+ **t :: Trail = [PathChoice DirID DirContent]** is a list of values of the form **[PathChoice d_1 dCont_1, ..., PathChoice d_n dCont_n]** (with n >= 0), where **d_1** is the ID of the parent directory of **d** and **dCont_1** is a list of of all the files and directories in **d1** except for **d**. The other **PathChoice**s in **t**, namely **[PathChoice d_2 dCont_2, ..., PathChoice d_n dCont_n]**, contain the required information to reconstruct the entire file system all the way up to its root.
 
*Note: Modelling file systems with a data structure like **PathChoice**, which falls under the category of [zippers](http://learnyouahaskell.com/zippers), allows for (arguably) simpler implementations of commands like **cd** and **cdup**, since (re)constructing directories can be trivially achieved via the data structure.*

\
The VSFS manager accepts the following commands:

+ **pwd :: FS -> Path** takes a file system **fs1** as argument. It returns a string representing the path from the root of **fss1** to its current directory.

+ **addFile :: FS -> File -> Maybe FS** takes a file system **fs1** and a file **f** as arguments. If **f** is already in the current directory of **fs1**, then **addFile fs1 f** returns **Nothing** (error). Otherwise, **addFile fs1 f** returns **Just fs2** (successful computation), where **fs2** is the result of adding a new file with ID **f** to the current directory of **fs1**.

+ **addDir :: FS -> Directory -> Maybe FS** takes a file system **fs1** and a directory **d** as arguments. If a directory with the same name as **d** is already in the current directory of **fs1**, then **addDir fs1 d** returns **Nothing** (error). Otherwise, **addDir fs1 d** returns **Just f2** (successful computation), where **f2** is the result of adding directory **d** to the current directory of **fs1**.

+ **cd :: FS -> DirID -> Maybe FS** takes a file system **fs1** and a directory ID **d_id** as arguments. If no directory with ID **d_id** exists in the current directory of **fs1**, then **cd fs1 d_id** returns **Nothing** (error). Otherwise, **cd fs1 d_id** returns **Just f2** (successful computation), where **f2** is the result of switching the current directory of **fs1** to the subdirectory whose ID is **d_id**.

+ **cdup :: FS -> Maybe FS** takes a file system **fs1** as argument. If the current directory of **fs1** is the root of the file system, then **cdup fs1** returns **Nothing**. Otherwise, it returns **Just f2**, where **f2** is exactly like **fs1** except that the current directory has been changed to the one right above.

+ **find :: FS -> String -> Maybe [Path]** takes a file system **fs1** and a string **s** as arguments, and returns **Just [p_1, ..., p_n]** (with n >= 0), where each **p_i** is an entire path from the root of the file system down to a file whose ID is **s**.

+ **rmFile :: FS -> Path -> Maybe FS** takes a file system **fs1** and a path **p** as arguments. If **p** does not lead to a file starting from the current directory of **fs1**, then **rmFile fs1 p** returns **Nothing**. Otherwise, it returns **Just fs2**, where **f2** is exactly like **fs1** except that the file corresponding to **p** has been removed.

\
Being its first implementation, VSFS should be run from within [GHCi](https://docs.haskellstack.org/en/stable/ghci/). Here is an example of a session of VSFS in GHCi:

	:l VSFS.hs

	let extract = (\(Just x) -> x) 

	let fs1 = (Directory Root [], [])
	let fs2 = addFile fs1 "file1"
	let fs3 = addDir (extract fs2) (Directory (NonRoot "dir1") [])
	let fs4 = cd (extract fs3) (NonRoot "dir1")
	pwd $ extract fs4
	let fs5 = cdup (extract fs4)
	pwd $ extract fs5


\
In the near future, I plan on handling errors via monads (leveraging the use of the Maybe monad) and extending the functionality provided by the accepted commands (with other useful commands like **rm**, or by generalizing **cd :: FS -> DirId -> Maybe FS** to **cd :: FS -> Path -> Maybe FS**), etc.
