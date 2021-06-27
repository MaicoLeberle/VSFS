module VSFS where

import Data.List.Split (splitOn)

import Text.Pretty.Simple (pPrint)

import Control.Monad.RWS.Lazy 
    -- This is used for the user interface, via Sessions.



data Command = 
    AddDir String 
    | AddFile String 
    | RmFile String
    | Cd String 
    | CdUp
    | Pwd
    | Ls
    | Find String


---
type Path = String

data File = File String
    deriving (Show, Eq)
data DirID = Root | NonRoot String
    deriving (Show, Eq)
---


---
data Directory = Directory DirID DirCont
    deriving (Eq, Show)

type DirCont = [Resource]
type Resource = Either File Directory
---


---
{- The filesystem, implemented as a zipper data structure, consists of a pair, 
the first coordinate being the current directory and the second coordinate 
containing everything needed to easily reconstruct all the FS, both upwards 
and downwards. -}
type FS = (Directory, Trail) 
{- TO DO: refactor FS as a data type and define
    instance Show FS where
        show fs = pPrint fs
-}
---


---
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
    deriving (Show, Eq)
---


---
type Session = RWS () String FS
{-  Sessions are implemented via the lazy version of the Reader Writer Transformer 
    monad, as imported from Control.Monad.RWS.Lazy. The type parameters are
        * the environment type, r, is the unit (),
        * the output type, w, is String, and
        * the updatable state type, s, is our file system type, FS.
-} 
---


---
{- Definitions of error types, for error reports during FSs manipulation. -}
type AddError = String
type AddResult = Either AddError FS

type FindError = String
type FindResult = Either FindError (FS, [Path])

type RemoveError = String
type RemoveResult = Either RemoveError FS

type CdError = String
type CdResult = Either CdError FS

type CdUpError = String
type CdUpResult = Either CdUpError FS
---


initFS :: FS
initFS = (Directory Root [], [])


runSession :: Session a -> (FS, String)
runSession fsa = execRWS fsa () initFS


{-  The usage is very simple: to execute Session ex1, simple execute
    runSession ex1 :: (FS, String).
-}
---


---
pwdMonadic :: Session ()
pwdMonadic = get >>= tell . pwd


pwd :: FS -> Path
pwd (_, []) = "/"
pwd (dir, trail) = 
    "/" ++ (concat (map pwdAux (reverse trail))) ++ (getNameDir dir)

pwdAux :: PathChoice -> Path
pwdAux (PathChoice Root _) = ""
pwdAux (PathChoice (NonRoot dirId) _) = dirId ++ "/"
---


---
ls :: FS -> [String]
ls (Directory dirID dirCont, trail) = map getNameResource dirCont
    where 
        getNameResource :: Resource -> String
        getNameResource (Right dir) = "(d)" ++ getNameDir dir
        getNameResource (Left file) = getNameFile file

lsMonadic :: Session ()
lsMonadic = get >>=
    \fs -> mapM_ tell $ mapNotLast (\x -> x ++ "\n") $ ls fs 


mapNotLast :: Eq a => (a -> a) -> [a] -> [a]
mapNotLast f [] = []
mapNotLast f (x:xs) 
    | xs == [] = [x]
    | otherwise = (f x) : (mapNotLast f xs)
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

getDirID :: Directory -> String
getDirID (Directory Root _) = "/"
getDirID (Directory (NonRoot res) _) = res

getNameDir :: Directory -> String
getNameDir dir = 
    case getIdDir dir of
        Root -> "/"
        NonRoot name -> name

getDirById :: DirCont -> DirID -> Maybe Directory
getDirById [] _ = Nothing
getDirById (head : tail) dirId =
    case head of
        (Left _) -> getDirById tail dirId
        (Right dir@(Directory headId dirContent)) ->
            if dirId == headId
            then Just dir
            else getDirById tail dirId

getNameFile :: File -> String
getNameFile (File file) = file
---


---
addFileMonadic :: File -> Session ()
addFileMonadic file = get >>= 
    \fs -> 
        case addFile fs file of
            Left err -> tell err
            Right newFs -> put newFs


addFile :: FS -> File -> AddResult
addFile fs@(Directory dirId dirCont, trail) file@(File fileName) 
    | shallowFindFiles fs fileName = 
        Left $ "File " ++ fileName ++ " already exists."
    | otherwise = 
        Right (Directory dirId (dirCont ++ [Left file]), trail)
---


-- ---
rmFileMonadic :: String -> Session ()
rmFileMonadic file = get >>=
    \fs ->
        case rmFile fs file of
            Left err -> tell err
            Right newFs -> put newFs


rmFile :: FS -> String -> RemoveResult
rmFile fs@(dir, trail) path = 
    let decomposedPath = (filter (/= "") . splitOn "/") path in 
    case rmFileAux dir decomposedPath of
        Left err -> 
            if length decomposedPath == 1
            then Left $ "File " ++ path ++ " does not exist."
            else Left $ "File at " ++ path ++ " does not exist."
        Right newDir -> Right (newDir, trail)


rmFileAux :: Directory -> [String] -> Either String Directory
rmFileAux dir [] = Right dir
rmFileAux dir@(Directory dirID dirCont) (head:tail) = 
    case tail of
        [] -> -- We have reached the directory where the file should be found.
            if fileIsInContent head dirCont
            then Right $ Directory dirID $ filterFiles head dirCont
            else Left "File not found in final destination."
        _ -> -- Path has not yet been fully consumed.
            if dirIsInContent head dirCont 
            then 
                let Just dirToUpdate = getDirById dirCont $ NonRoot head in
                case rmFileAux dirToUpdate tail of
                    Left err -> Left err
                    Right updatedDir -> Right $ Directory dirID $ replaceDir dirCont head updatedDir
            else Left "Path does not exist."

    where
        replaceDir :: DirCont -> String -> Directory -> DirCont
        replaceDir dirCont dirName newDir = 
            map (\res -> 
                case res of
                    Left file -> Left file
                    Right oldDir -> Right $ if dirName == getNameDir oldDir then newDir else oldDir) dirCont

        filterFiles :: String -> DirCont -> DirCont
        filterFiles name [] = []
        filterFiles name (x:xs) = 
            case x of 
                Left (File y) -> 
                    if name == y
                    then xs
                    else x : filterFiles name xs
                y@(_) -> y : filterFiles name xs

        fileIsInContent :: String -> [Resource] -> Bool
        fileIsInContent fileName [] = False
        fileIsInContent fileName (x:xs) = 
            case x of
                Left (File name) -> 
                    if fileName == name 
                    then True
                    else fileIsInContent fileName xs
                _         -> fileIsInContent fileName xs

        dirIsInContent :: String -> [Resource] -> Bool
        dirIsInContent dirName [] = False
        dirIsInContent dirName (x:xs) = 
            case x of
                Right dir@(Directory _ _) -> 
                    if dirName == getDirID dir 
                    then True
                    else dirIsInContent dirName xs
                _         -> dirIsInContent dirName xs
-- ---


---
addDirMonadic :: Directory -> Session ()
addDirMonadic dir = get >>=
    \fs ->
        case addDir fs dir of
            Left err -> tell err
            Right newFs -> put newFs


addDir :: FS -> Directory -> AddResult
addDir fs@(Directory dirId dirCont, trail) dir
    | shallowFindDirs fs dir = 
        Left $ "Directory " ++ (getDirID dir) ++ " already exists." 
    | otherwise = 
        Right (Directory dirId (dirCont ++ [Right dir]), trail)
---


---
removeDir :: DirID -> DirCont ->  DirCont
removeDir dirId [] = []
removeDir dirId (head : tail) = 
    case head of
        file@(Left _) -> file : (removeDir dirId tail)
        dir@(Right (Directory id cont)) -> 
            (if dirId == id
            then removeDir dirId tail
            else dir : (removeDir dirId tail))
---


---
cd :: FS -> DirID -> CdResult
cd _ Root = Left "Cannot change directory to root."
cd fs@(dir@(Directory dirId dirCont),trail) newDirId@(NonRoot newDir) = 
    if not $ elem newDir (map getNameDir (getLocalDirs fs))
    then Left $ "Directory " ++ newDir ++ " does not exist."
    else Right $ ((\(Just x) -> x) (getDirById dirCont newDirId), 
               (PathChoice dirId $ removeDir newDirId dirCont) : trail)

cdMonadic :: DirID -> Session ()
cdMonadic dirId = get >>=
    \fs ->
        case cd fs dirId of
            Left err -> tell err
            Right newFs -> put newFs
---


---
cdUp :: FS -> CdUpResult
cdUp (_, []) = Left $ "Already in root directory."
cdUp (currentDir, (PathChoice parID parCont) : trail) = 
    Right $ (Directory parID ((Right currentDir) : parCont), trail)


cdUpMonadic :: Session()
cdUpMonadic = get >>= 
    \fs -> 
        case cdUp fs of
            Left err -> tell err
            Right newFs -> put newFs
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


{- Iterate shallowFindFiles down the directory structure, in a depth first 
search fashion. We implement the DFS stack as a list of lists of 
Directorys, and pass it on to findAux. -}
findAux :: FS -> String -> [[String]] -> FindResult
findAux fs fileName listDirs = 
    case listDirs of 
        [] -> Right (fs, [])
        head : tail -> 
            case head of 
                [] ->
                    let localCheck = (if shallowFindFiles fs fileName 
                                      then Right $ (printPath fs fileName) 
                                      else Left "") in
                    let recCheck = (if isRoot fs 
                                    then Right (fs,[])
                                    else 
                                        case cdUp fs of
                                            Left err -> Left err
                                            Right updatedFs -> findAux updatedFs fileName tail) in
                        case (localCheck, recCheck) of
                            (_, Left _) -> Left $ "ERROR"
                            (Left _, Right recResult) -> Right recResult
                            (Right localFile, Right (_, recPaths)) -> Right (fs, localFile : recPaths)
                dirName : dirsTail -> 
                    case cd fs (NonRoot dirName) of
                        Left msg -> Left msg
                        Right updatedFs -> 
                            findAux updatedFs fileName ((map getNameDir $ getLocalDirs updatedFs) : (dirsTail : tail))
    where
    isRoot :: FS -> Bool
    isRoot (Directory Root _, _) = True
    isRoot _                    = False

    printPath :: FS -> String -> String
    printPath (Directory Root _, _) fileName = "/" ++ fileName
    printPath _                     fileName = (pwd fs) ++ "/" ++ fileName


find :: FS -> String -> FindResult
find fs@(Directory _ dirCont, _) fileName = 
    case findAux fs fileName [map getNameDir $ getLocalDirs fs] of
        Left err -> Left err
        Right (auxFs, paths) -> Right $ (fs, paths) -- I discard auxFs and restore fs. 


findMonadic :: String -> Session ()
findMonadic fileName = get >>=
    \fs -> 
        case find fs fileName of
            Left err -> tell err
            Right (newFs, path) -> 
                do
                    mapM_ tell (mapNotLast (\x -> x ++ "\n") path)
                    put newFs
---

