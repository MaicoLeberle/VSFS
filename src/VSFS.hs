---
type Path = String

data File = File String
    deriving Show
data DirID = Root | NonRoot String
    deriving (Show, Eq)
data Directory = Directory DirID DirCont
    deriving Show

type Resource = Either File Directory
type DirCont = [Resource]
---


{- The filesystem, implemented as a zipper data structure, consists of a pair, 
the first coordinate being the current directory and the second coordinate 
containing everything needed to easily reconstruct all the FS, both upwards 
and downwards. -}
type FS = (Directory, Trail) 

type Trail = [PathChoice]
{- PathChoice's are used for implementing the zipper data structure with which 
we traverse the filesystem. It allows us to go up and down the filesystem 
structure, thus changing the current directory, in an efficient way.
The values in a PathChoice reprensent the following:
    - DirID: name of the parent directory.
    - DirCont: all other resources appearing as content in the parent
    directory *except for* the current directory of the filesystem, which 
    appears in the first coordinate in the definition of type FS above. -}
data PathChoice = PathChoice DirID DirCont
    deriving Show
---


---
pwd :: FS -> Path
pwd (_, []) = "/"
pwd (dir, trail) = 
    "/" ++ (concat (map pwdAux trail)) ++ (getNameDir dir)

pwdAux :: PathChoice -> Path
pwdAux (PathChoice Root _) = ""
pwdAux (PathChoice (NonRoot dirId) _) = dirId ++ "/"
---


---
getLocalDirs :: FS -> [Directory]
getLocalDirs (Directory dirId dirCont, trail) = 
    case dirCont of
        [] -> []
        (head : tail) ->
            case head of
                (Left _) -> getLocalDirs (Directory dirId tail, trail)
                (Right dir) -> dir : (getLocalDirs (Directory dirId tail, trail))
---


---
getLocalFiles :: FS -> [File]
getLocalFiles (Directory dirId dirCont, trail) =
    case dirCont of
        [] -> []
        (head : tail) ->
            case head of
                (Left file) -> file : (getLocalFiles (Directory dirId tail, trail))
                (Right _) -> getLocalFiles (Directory dirId tail, trail)
---


---
getIdDir :: Directory -> DirID
getIdDir (Directory dirId _) = dirId

getNameDir :: Directory -> String
getNameDir dir = 
    case getIdDir dir of
        Root -> "/"
        NonRoot name -> name

getDirById :: DirCont -> DirID -> Maybe Directory
getDirById [] _ = Nothing
getDirById (head : tail) dirId =
    case head of
        (Left _) -> Nothing
        (Right dir@(Directory headId dirContent)) ->
            if dirId == headId
            then Just dir
            else getDirById tail dirId

getNameFile :: File -> String
getNameFile (File file) = file
---


---
addFile :: FS -> String -> Maybe FS
addFile fs@(Directory dirId dirCont, trail) fileName = 
    if shallowFindFiles fs fileName
    then Nothing
    else Just (Directory dirId (Left (File fileName) : dirCont), trail)
---


---
addDir :: FS -> Directory -> Maybe FS
addDir fs@(Directory dirId dirCont, trail) dir =
    if shallowFindDirs fs dir
    then Nothing
    else Just (Directory dirId (Right dir : dirCont), trail)
---


---
removeDir :: DirID -> DirCont ->  DirCont
removeDir dirId [] = []
removeDir dirId (head : tail) = 
    case head of
        file@(Left _) -> file : (removeDir dirId tail)
        dir@(Right (Directory id cont)) -> (if dirId == id
                                            then removeDir dirId tail
                                            else dir : (removeDir dirId tail))

cd :: FS -> DirID -> Maybe FS
cd _ Root = Nothing
cd fs@(dir@(Directory dirId dirCont),trail) newDirId@(NonRoot newDir) = 
    if not $ elem newDir (map getNameDir (getLocalDirs fs))
    then Nothing
    else Just ((\(Just x) -> x) (getDirById dirCont newDirId), 
               (PathChoice dirId $ removeDir dirId dirCont) : trail)
---


---
cdup :: FS -> Maybe FS
cdup (_, []) = Nothing
cdup (currentDir, (PathChoice parID parCont) : trail) = 
    Just (Directory parID ((Right currentDir) : parCont), trail)
---


---
{- Search for a file in the FS's current directory. -}
shallowFindFiles :: FS -> String -> Bool
shallowFindFiles fs fileName = 
    elem fileName (map getNameFile (getLocalFiles fs))

{- Search for a directory in the FS's current directory. -}
shallowFindDirs :: FS -> Directory -> Bool
shallowFindDirs fs dir = 
    elem (getNameDir dir) (map getNameDir (getLocalDirs fs))
---


{- Iterate shallowFindFiles down the directory structure, in a depth first 
search fashion. We implement the DFS stack with a list of lists of 
Directory, and pass it on to findAux. -}
find :: FS -> String -> Maybe [Path]
find fs@(Directory _ dirCont, _) fileName = 
    findAux fs fileName [map getNameDir $ getLocalDirs fs]

findAux :: FS -> String -> [[String]] -> Maybe [Path]
findAux fs fileName listDirs = 
    case listDirs of 
        [] -> Just []
        head : tail -> 
            case head of 
                [] -> 
                    let localCheck = (if shallowFindFiles fs fileName 
                                      then Just ((pwd fs) ++ "/" ++ fileName) 
                                      else Nothing) in
                    let recCheck = cdup fs >>= (\updatedFs -> findAux updatedFs fileName tail) in
                        case (localCheck, recCheck) of
                            (_, Nothing) -> Nothing
                            (Nothing, Just recPaths) -> Just recPaths
                            (Just localFile, Just recPaths) -> Just (localFile : recPaths)
                dirName : dirsTail -> 
                    case cd fs (NonRoot dirName) of
                        Nothing -> Nothing
                        Just updatedFs -> 
                            findAux updatedFs fileName ((map getNameDir $ getLocalDirs updatedFs) : (dirsTail : tail))
---