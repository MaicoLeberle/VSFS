module Types where

import Control.Monad.RWS.Lazy


data ResultAlternative = V VSFSMan | S (VSFSMan, VSFSMan) | L [String]
  deriving (Eq, Show)

type NamedFS = (String, FS)

{-| The filesystem, implemented as a zipper data structure, consists of a pair,
    the first coordinate being the current directory and the second coordinate
    containing everything needed to easily reconstruct all the FS, both upwards
    and downwards. -}

type FS = (Directory, [PathChoice])

{-| PathChoices are used for implementing the zipper data structure with which
    we traverse the filesystem. It allows us to go up and down the filesystem
    structure, thus changing the current directory, in an efficient way.
    The values in a PathChoice reprensent the following:
        - DirID: name of the parent directory.
        - DirCont: all other resources appearing as content in the parent
    directory *except for* the current directory of the filesystem, which
    appears in the first coordinate in the definition of type FS above.
-}
data PathChoice = PathChoice DirID DirCont
  deriving (Eq, Show)

data DirID = Root | NonRoot String
  deriving (Eq, Show)

type DirCont = [Resource]

data Resource = File String | Dir Directory
  deriving (Eq, Show)

data Directory = Directory DirID DirCont
  deriving (Eq, Show)

{-| The VSFSMan type represents an application run, managing multiple file
    systems.
-}
type VSFSMan = [NamedFS]

data ManCommand = List | Init String | Switch String | Delete String
  deriving (Eq, Show)

data Command =
      Pwd
    | Ls
    | MkDir String
    | Cd String
    | CdUp
    | AddFile String
    | RmFile String
    | Find String
  deriving (Eq, Show)

{-| Stateful computations are implemented via the (lazy version of the) RWS
    monad. In particular,
        * String is the output type, and
        * FS is the updatable state type.
-}
type Session = RWS () String FS ()
