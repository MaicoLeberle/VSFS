module VSFS where

import Data.Either

import Data.List.Split (splitOn)

import Text.Pretty.Simple (pPrint)

import Control.Monad.RWS.Lazy 
    -- this is used to represent file system sessions.




{-  IMPLEMENTATION OF THE FILE SYSTEMS MANAGER.
    (See below for the implementation of file systems themselves. -}
---
type FSName = String
type FSUnit = (FSName, FS)
type VSFSMan = [FSUnit]
{-  The VSFSMan type represents an application run, managing multiple file
    systems. 
-}
---


---
data ManCommand = 
    List 
    | Init String
    | Switch String
    | Delete String
---


---
{-  Definition of error types, for error reports during VSFSMan 
    manipulation. 
-}
type InitError = String
type InitResult = Either InitError VSFSMan

type SwitchError = String
type SwitchResult = Either SwitchError (VSFSMan, VSFSMan)
    -- Note the right type here: if the file system has been found, the 
    -- we split the VSFSMan value in two, where the head of the second 
    -- coordinate is the found file system.

type DeleteError = String
type DeleteResult = Either DeleteError VSFSMan


initMan :: VSFSMan
initMan = []
---



---
list :: VSFSMan -> IO ()
list = sequence_ . (map (putStrLn . fst))
---


---
initialize :: VSFSMan -> FSName -> InitResult  
initialize manager fsName 
    | elem fsName $ map fst manager = 
        Left $ "A file system called " ++ fsName ++ " already exists."
    | otherwise = Right $ (fsName, initFS) : manager
---


---
switch :: VSFSMan -> FSName -> SwitchResult
switch manager fsName 
    | not $ elem fsName $ map fst manager = 
        Left $ "No file system called " ++ fsName ++ " has been found."
    | otherwise = manager `splitAt` fsName

    where 
        splitAt :: VSFSMan -> FSName -> SwitchResult
        [] `splitAt` fsName = 
            Left $ "No file system called " ++ fsName ++ " has been found."
        man@(x:xs) `splitAt`fsName 
            | fst x == fsName = Right $ ([], man)
            | otherwise = 
                case xs `splitAt` fsName of 
                    Left err -> Left err
                    Right auxMan -> Right $ (x : fst auxMan, snd auxMan)
---


---
delete :: VSFSMan -> FSName -> DeleteResult
delete [] fsName = Left $ "No file system called " ++ fsName ++ " has been found."
delete (x:xs) fsName 
    | fst x == fsName = Right $ xs
    | otherwise =
        case delete xs fsName of
            Left err -> Left err
            Right newMan -> Right $ x : newMan
---




{- IMPLEMENTATION OF THE COMMANDS SUPPORTED BY A FILE SYSTEM. -}

---
type Path = String
type Resource = Either File Directory

data File = File String deriving (Show)

data DirID = Root | NonRoot String deriving (Show, Eq)
type DirCont = [Resource]
data Directory = Directory DirID DirCont deriving (Show)
---


---
{-  PathChoices are used for implementing the zipper data structure with which 
    we traverse the filesystem. It allows us to go up and down the filesystem 
    structure, thus changing the current directory, in an efficient way.
    The values in a PathChoice reprensent the following:
        - DirID: name of the parent directory.
        - DirCont: all other resources appearing as content in the parent
    directory *except for* the current directory of the filesystem, which 
    appears in the first coordinate in the definition of type FS above. 
-}
data PathChoice = PathChoice DirID DirCont deriving (Show)
type Trail = [PathChoice]
---


---
{- The filesystem, implemented as a zipper data structure, consists of a pair, 
the first coordinate being the current directory and the second coordinate 
containing everything needed to easily reconstruct all the FS, both upwards 
and downwards. -}
type FS = (Directory, Trail) 

initFS :: FS
initFS = (Directory Root [], [])
---


---
type Session = RWS () String FS
{-  Stateful computations are implemented via the lazy version of the RWS 
    monad. The type parameters are
        * () is the the environment type,
        * String is the output type, and
        * FS is the updatable state type.
-} 

runSession :: Session a -> FS -> (FS, String)
runSession session fs = execRWS session () fs 
---


---
data Command = 
    AddDir String 
    | AddFile String 
    | RmFile String
    | RmDir String 
        -- this command is not implemented yet.
    | Cd String 
    | CdUp
    | Pwd
    | Ls
    | Find String
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



---
{- First, some getter functions to work around with. -}
getLocalDirs :: FS -> [Directory]
getLocalDirs (Directory dirId dirCont, trail) = 
    case dirCont of
        [] -> []
        (head : tail) ->
            case head of
                (Left _) -> getLocalDirs (Directory dirId tail, trail)
                (Right dir) -> 
                    dir : (getLocalDirs (Directory dirId tail, trail))
 
getLocalFiles :: FS -> [File]
getLocalFiles (Directory dirId dirCont, trail) =
    case dirCont of
        [] -> []
        (head : tail) ->
            case head of
                (Left file) -> 
                    file : (getLocalFiles (Directory dirId tail, trail))
                (Right _) -> getLocalFiles (Directory dirId tail, trail)

getDirID :: Directory -> DirID
getDirID (Directory dirId _) = dirId

getDirName :: Directory -> String
getDirName dir = 
    case getDirID dir of
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

getFileName :: File -> String
getFileName (File file) = file
---






{- IMPLEMENTATION OF THE COMMANDS SUPPORTED BY EACH FILE SYSTEM. -}

---
{- COMMAND: pwd -}

{- Get the FS encapsulated in the RWS monad and apply pwd to it. -}
pwdMonadic :: Session ()
pwdMonadic = get >>= tell . pwd


{-  Note: reverse is required because PathChoices (in a FS's Trail coordinate) 
    are stacked in such a way that the innermost parent directory appears 
    first (in the Trail of the FS). -}
pwd :: FS -> Path
pwd (_, []) = "/"
pwd (dir, trail) = 
    "/" ++ (concat (map pwdAux (reverse trail))) ++ (getDirName dir)
    where
        pwdAux :: PathChoice -> Path
        pwdAux (PathChoice Root _) = ""
        pwdAux (PathChoice (NonRoot dirId) _) = dirId ++ "/"
---



---
{- COMMAND: ls -}

{-  Get the FS encapsulated in the RWS monad and apply ls to it, printing the 
    result in the right way. -}
lsMonadic :: Session ()
lsMonadic = get >>=
        \fs -> 
            mapM_ tell $ mapNotLast (flip (++) $ "\n") $ ls fs


{- Content of the current directory can be trivially retrieved.-}
ls :: FS -> [String]
ls (Directory dirID dirCont, trail) = map getNameResource dirCont
    where 
        getNameResource :: Resource -> String
        getNameResource (Right dir) = "(d)" ++ getDirName dir
        getNameResource (Left file) = getFileName file

mapNotLast :: (a -> a) -> [a] -> [a]
mapNotLast f [] = []
mapNotLast f xs = (map f (init xs)) ++ [last xs]
---



---
{- COMMAND: ls -}

{-  Get the FS encapsulated in the RWS monad and apply addFile to it.
    If there is an error, print it. Otherwise, update the state (i.e., the 
    FS). 
-}
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


{- Search for a file in the FS's current directory. -}
shallowFindFiles :: FS -> String -> Bool
shallowFindFiles fs fileName = 
    elem fileName (map getFileName (getLocalFiles fs))
---



---
{- COMMAND: rmFile -}

{-  Get the FS encapsulated in the RWS monad and apply rmFile to it.
    If there is an error, print it. Otherwise, update the state (i.e., the 
    FS), by removing the file. 
-}
rmFileMonadic :: String -> Session ()
rmFileMonadic file = get >>=
    \fs ->
        case rmFile fs file of
            Left err -> tell err
            Right newFs -> put newFs


rmFile :: FS -> Path -> RemoveResult
rmFile fs@(dir, trail) path = 
    let decomposedPath = (filter (/= "") . splitOn "/") path in 
    case rmFileAux dir decomposedPath of
        Left err -> 
            if length decomposedPath == 1
            then Left $ "File " ++ path ++ " does not exist."
            else Left err
        Right newDir -> Right (newDir, trail)


rmFileAux :: Directory -> [String] -> Either String Directory
rmFileAux dir [] = Right dir
rmFileAux dir@(Directory dirID dirCont) (localDir:restOfPath) = 
    case restOfPath of
        [] -> -- We have reached the directory where the file should be found.
            if fileIsInContent localDir dirCont
            then 
                Right 
                    $ Directory dirID 
                    $ filterFiles localDir dirCont
            else 
                Left "File not found in final destination."
        _ -> -- Path has not yet been fully consumed.
            if dirIsInContent localDir dirCont 
            then 
                let Just dirToUpdate = getDirById dirCont $ NonRoot localDir in
                case rmFileAux dirToUpdate restOfPath of
                    Left err -> Left err
                    Right updatedDir -> 
                        Right $ 
                            Directory dirID $ 
                                replaceDir dirCont localDir updatedDir
            else Left "Path does not exist." -- head 

    where
        replaceDir :: DirCont -> String -> Directory -> DirCont
        replaceDir dirCont dirName newDir = 
            map (\res -> 
                case res of
                    Left file -> Left file
                    Right oldDir -> 
                        Right $
                            if dirName == getDirName oldDir 
                            then newDir 
                            else oldDir) 
                dirCont


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
                Left file ->
                    fileName == getFileName file || fileIsInContent fileName xs
                _         -> fileIsInContent fileName xs

        dirIsInContent :: String -> [Resource] -> Bool
        dirIsInContent dirName [] = False
        dirIsInContent dirName (x:xs) = 
            case x of
                Right dir -> 
                    dirName == getDirName dir || dirIsInContent dirName xs
                _         -> dirIsInContent dirName xs
---



---
{- COMMAND: addDir -}

{-  Get the FS encapsulated in the RWS monad and apply addDir to it.
    If there is an error, print it. Otherwise, update the state (i.e., the 
    FS) by adding the new directory. 
-}
addDirMonadic :: Directory -> Session ()
addDirMonadic dir = get >>=
    \fs ->
        case addDir fs dir of
            Left err -> tell err
            Right newFs -> put newFs


addDir :: FS -> Directory -> AddResult
addDir fs@(Directory dirId dirCont, trail) dir
    | shallowFindDirs fs dir = 
        Left $ "Directory " ++ (getDirName dir) ++ " already exists." 
    | otherwise = 
        Right (Directory dirId (dirCont ++ [Right dir]), trail)
---



---
{- COMMAND: cd -}

{-  Get the FS encapsulated in the RWS monad and apply cd to it.
    If there is an error, print it. Otherwise, update the state (i.e., the 
    FS) by changing the current directory and extending the Trail coordinate
    of the FS accordingly. 
-}
cdMonadic :: DirID -> Session ()
cdMonadic dirId = get >>=
    \fs ->
        case cd fs dirId of
            Left err -> tell err
            Right newFs -> put newFs


cd :: FS -> DirID -> CdResult
cd _ Root = Left "Cannot change directory to root."
cd fs@(dir@(Directory dirId dirCont),trail) newDirId@(NonRoot newDir) = 
    if not $ elem newDir (map getDirName (getLocalDirs fs))
    then Left $ "Directory " ++ newDir ++ " does not exist."
    else Right $ ((\(Just x) -> x) (getDirById dirCont newDirId), 
               (PathChoice dirId $ removeDir newDirId dirCont) : trail)


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
{- COMMAND: cdup -}

{-  Get the FS encapsulated in the RWS monad and apply cdup to it.
    If there is an error, print it. Otherwise, update the state (i.e., the 
    FS) by changing the current directory to the immediate parent of the 
    current directory. 
-}
cdUpMonadic :: Session()
cdUpMonadic = get >>= 
    \fs -> 
        case cdUp fs of
            Left err -> tell err
            Right newFs -> put newFs


{-  NOTE: the zipper structure of FSs makes switching to the immediate parent 
    directory of the current one quite easy. -}
cdUp :: FS -> CdUpResult
cdUp (_, []) = Left $ "Already in root directory."
cdUp (currentDir, (PathChoice parID parCont) : trail) = 
    Right $ (Directory parID ((Right currentDir) : parCont), trail)
---



---
findMonadic :: String -> Session ()
findMonadic fileName = get >>=
    \fs -> 
        case find fs fileName of
            Left err -> tell err
            Right (newFs, path) -> 
                do
                    mapM_ tell (mapNotLast (\x -> x ++ "\n") path)
                    put newFs


find :: FS -> String -> FindResult
find fs@(Directory _ dirCont, _) fileName = 
    case findAux fs fileName [map getDirName $ getLocalDirs fs] of
        Left err -> Left err
        Right (auxFs, paths) -> 
            Right $ (fs, reverse paths) -- I discard auxFs and restore fs. 


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
                            (Right localFile, Right (_, recPaths)) -> 
                                Right (fs, localFile : recPaths)
                dirName : dirsTail -> case cd fs (NonRoot dirName) of
                    Left msg -> Left msg
                    Right updatedFs -> findAux
                        updatedFs
                        fileName 
                        ((map getDirName $ getLocalDirs updatedFs) 
                            : (dirsTail : tail))
    where
    isRoot :: FS -> Bool
    isRoot = ((==) Root) . getDirID . fst 

    printPath :: FS -> String -> String
    printPath (Directory Root _, _) fileName = "/" ++ fileName
    printPath _                     fileName = (pwd fs) ++ "/" ++ fileName


{- Search for a directory in the FS's current directory. -}
shallowFindDirs :: FS -> Directory -> Bool
shallowFindDirs fs dir = 
    elem (getDirName dir) (map getDirName (getLocalDirs fs))
---