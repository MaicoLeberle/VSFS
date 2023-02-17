{-# LANGUAGE LambdaCase #-}

module VSFS where

import Data.Either
import Data.List.Split        (splitOn)
import Text.Pretty.Simple     (pPrint)
import Control.Monad.RWS.Lazy

import Types


initMan :: VSFSMan
initMan = []

list :: VSFSMan -> [String]
list = map fst

initialize :: String -> VSFSMan -> Either String VSFSMan
initialize fs manager
    | elem fs $ map fst manager = Left $ fs ++ " already exists."
    | otherwise = Right $ (fs, initFS) : manager

switch :: String -> VSFSMan -> Either String (VSFSMan, VSFSMan)
switch fs man | elem fs $ map fst man = splitAtFileSystem man
              | otherwise = Left $ "File system " ++ fs ++ " not found."
  where
    splitAtFileSystem :: VSFSMan -> Either String (VSFSMan, VSFSMan)
    splitAtFileSystem [] = Left $ "File system " ++ fs ++ " not found."
    splitAtFileSystem man@(x:xs)
        | fst x == fs = Right ([], man)
        | otherwise = do
            (prefix, suffix) <- splitAtFileSystem xs
            return (x : prefix, suffix)

delete :: String -> VSFSMan -> Either String VSFSMan
f `delete` [] = Left $ "File system " ++ f ++ " not found."
f `delete` (x:xs) | fst x == f = Right $ xs
                  | otherwise = f `delete` xs >>= Right . (x:)

initFS :: FS
initFS = (Directory Root [], [])

runSession :: Session -> FS -> (FS, String)
runSession session fs = execRWS session () fs

-- | Commands supported by file systems.
pwdMonadic :: Session
pwdMonadic = get >>= tell . pwd

{-  Note: since the list of PathChoice's leading to a file systems's current
    directory are ordered in such a way that the innermost parent directory
    appears first, we call `reverse`. -}
pwd :: FS -> String
pwd (_, []) = "/"
pwd (dir, trail) =
    concat ["/", concat $ map pwdAux $ reverse trail, getResName $ Dir dir]
  where
    pwdAux :: PathChoice -> String
    pwdAux (PathChoice Root _) = ""
    pwdAux (PathChoice (NonRoot dirId) _) = dirId ++ "/"

lsMonadic :: Session
lsMonadic = get >>= mapM_ tell . mapInit (++ ['\n']) . ls

ls :: FS -> [String]
ls (Directory _ dirCont, _) = map getName dirCont
  where
    getName :: Resource -> String
    getName d@(Dir _) = "(d)" ++ getResName d
    getName f = getResName f

rmDirMonadic :: String -> Session
rmDirMonadic id = do
    fs@(Directory current content, trail) <- get
    case id `shallowFindDir` content of
        Just d -> put ( Directory current (filterDir (NonRoot id) content)
                      , trail
                      )
        Nothing -> tell $ "Directory " ++ show id ++ " not found."
  where
    filterDir :: DirID -> DirCont -> DirCont
    filterDir dId [] = []
    filterDir dId (dir@(Dir (Directory dId' _)):xs)
        | dId == dId' = xs
        | otherwise   = dir : filterDir dId xs
    filterDir dId (f : xs) = f : filterDir dId xs

{-| Search for a directory in the file system's current directory. -}
shallowFindDir :: String -> DirCont -> Maybe Directory
_ `shallowFindDir` [] = Nothing
id `shallowFindDir` (File _ : rs) = id `shallowFindDir` rs
id `shallowFindDir` (Dir d@(Directory id' _): rs)
    | NonRoot id == id' = Just d
    | otherwise = id `shallowFindDir` rs

{-  Get the FS encapsulated in the RWS monad and apply `mkDir` to it.
    If there is an error, print it. Otherwise, update the state (i.e., the
    FS) by adding the new directory.
-}
mkDirMonadic :: String -> Session
mkDirMonadic id = do
    fs@(Directory id' dirCont, trail) <- get
    case id `shallowFindDir` dirCont of
        Just  _ -> tell $ "Directory " ++ show id ++ " already exists."
        Nothing ->
            put ( Directory id' (dirCont ++ [Dir (Directory (NonRoot id) [])])
                , trail
                )

{-| `get` the file system, apply pure function `cd` to it, and `put` the result.
    Report any possible errors.
-}
cdMonadic :: DirID -> Session
cdMonadic dirId = do
    fs <- get
    case cd fs dirId of
        Left err -> tell err
        Right newFs -> put newFs

{-| Update the current directory of the fiel system and extend the list of
    PathChoice's by adding the former current directory.
-}
cd :: FS -> DirID -> Either String FS
cd _ Root = Left "Cannot change directory to root."
cd fs@(dir@(Directory dirId dirCont),trail) newDirId@(NonRoot newDir) =
    if elem newDir $ map (getResName . Dir) $ getLocalDirs fs
    then let Just dir = getDirById dirCont newDirId
         in Right (dir, (PathChoice dirId $ removeDir newDirId dirCont) : trail)
    else Left $ "Directory " ++ newDir ++ " could not be found."

removeDir :: DirID -> DirCont ->  DirCont
removeDir dirId [] = []
removeDir dirId (head : tail) = case head of
    file@(File _) -> file : (removeDir dirId tail)
    dir@(Dir (Directory id cont)) | dirId == id -> removeDir dirId tail
                                  | otherwise   -> dir : (removeDir dirId tail)

{-  Apply cdUp on the current file system. If there is an error, tell it.
    Otherwise, update the file system by changing the focused directory to the
    immediate parent of the current one.
-}
cdUpMonadic :: Session
cdUpMonadic = do
    fs <- get
    case cdUp fs of
        Left err    -> tell err
        Right newFs -> put newFs

{-  NOTE: the zipper structure of FSs makes switching to the immediate parent
    directory of the current one quite easy. -}
cdUp :: FS -> Either String FS
cdUp (_, []) = Left $ "Already in root directory."
cdUp (currentDir, (PathChoice parID parCont) : trail) =
    Right $ (Directory parID $ (Dir currentDir) : parCont, trail)

{-  Get the FS encapsulated in the RWS monad and apply addFile to it.
    If there is an error, print it. Otherwise, update the file system.
-}
addFileMonadic :: String -> Session
addFileMonadic f = do
    fs@(Directory dirId dirCont, trail) <- get
    if f `shallowFindFile` fs
        then tell $ "File " ++ f ++ " already exists."
        else put (Directory dirId (dirCont ++ [File f]), trail)

{- Search for a file in the current directory. -}
shallowFindFile :: String -> FS -> Bool
shallowFindFile f fs = elem f (map getResName (getLocalFiles fs))

{-  Get the FS encapsulated in the RWS monad and apply rmFile to it.
    If there is an error, print it. Otherwise, update the state (i.e., the
    FS), by removing the file.
-}
rmFileMonadic :: String -> Session
rmFileMonadic file = do
    fs <- get
    case rmFile fs file of
        Left err -> tell err
        Right newFs -> put newFs

rmFile :: FS -> String -> Either String FS
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
rmFileAux dir@(Directory dirID dirCont) (currDir:tail) = case tail of
    -- We have reached the directory where the file should be found.
    [] | fileInDirCont currDir dirCont ->
            Right $ Directory dirID $ filterFiles currDir dirCont
       | otherwise -> Left "File not found in final destination."
    -- Path has not yet been fully consumed.
    _ | dirInDirCont currDir dirCont ->
            case getDirById dirCont $ NonRoot currDir of
                Just dirToUpdate ->
                    rmFileAux dirToUpdate tail >>=
                        return . Directory dirID . replaceDir dirCont currDir
                Nothing -> Left "Error while removing file."
      | otherwise -> Left "Path does not exist."
    where
        replaceDir :: DirCont -> String -> Directory -> DirCont
        replaceDir dirCont dir newDir = map aux dirCont
          where
            aux :: Resource -> Resource
            aux = \case
                f@(File _) -> f
                d@(Dir oldDir) | dir == getResName d -> Dir newDir
                               | otherwise           -> Dir oldDir

        filterFiles :: String -> DirCont -> DirCont
        filterFiles name [] = []
        filterFiles name (x:xs) =
            case x of
                File y | name == y -> xs
                       | otherwise -> x : filterFiles name xs
                d@(_) -> d : filterFiles name xs

        fileInDirCont :: String -> DirCont -> Bool
        fileInDirCont f [] = False
        fileInDirCont f (f'@(File _) : xs) | f == getResName f' = True
                                           | otherwise = fileInDirCont f xs
        fileInDirCont f (_ : xs) = fileInDirCont f xs

        dirInDirCont :: String -> DirCont -> Bool
        dirInDirCont d [] = False
        dirInDirCont d (d'@(Dir _) : xs) | d == getResName d' = True
                                         | otherwise = dirInDirCont d xs
        dirInDirCont d (_ : xs) = dirInDirCont d xs

findMonadic :: String -> Session
findMonadic f = do
    fs@(Directory _ dirCont, _) <- get
    case findAux f [map (getResName . Dir) $ getLocalDirs fs] fs of
        Left err -> tell err
        Right (_, paths) -> mapM_ tell (mapInit (++ ['\n']) (reverse paths))

{-| Iterate shallowFindFile down the directory structure, in a depth first
    search fashion. We implement the DFS stack as a list of lists of Directorys,
    and pass it on to findAux. -}
findAux :: String -> [[String]] -> FS -> Either String (FS, [String])
findAux f [] fs = Right (fs, [])
findAux f ([] : tail) fs@(Directory dirId _, _) =
    let localCheck | f `shallowFindFile` fs = Right (printPath fs f)
                   | otherwise = Left ""
        recCheck | dirId == Root = Right (fs,[])
                 | otherwise = cdUp fs >>= findAux f tail
    in case (localCheck, recCheck) of
            (_, Left _) -> Left $ "ERROR"
            (Left _, Right recResult) -> Right recResult
            (Right localFile, Right (_, rec)) -> Right (fs, localFile : rec)
  where
    printPath :: FS -> String -> String
    printPath (Directory Root _, _) f = "/" ++ f
    printPath _                     f = pwd fs ++ "/" ++ f
findAux f ((dir : dirsTail) : tail) fs = do
    updFs <- cd fs (NonRoot dir)
    findAux f
            ((map (getResName . Dir) $ getLocalDirs updFs): (dirsTail : tail))
            updFs

-- | Getters.
getLocalDirs :: FS -> [Directory]
getLocalDirs (Directory _ [], _) = []
getLocalDirs (Directory dirId ((File _) : tail), trail) =
    getLocalDirs (Directory dirId tail, trail)
getLocalDirs (Directory dirId ((Dir dir) : tail), trail) =
    dir : (getLocalDirs (Directory dirId tail, trail))

getLocalFiles :: FS -> DirCont
getLocalFiles (Directory _ [], _) = []
getLocalFiles (Directory dirId (file@(File _) : tail), trail) =
    file : (getLocalFiles (Directory dirId tail, trail))
getLocalFiles (Directory dirId ((Dir _) : tail), trail) =
    getLocalFiles (Directory dirId tail, trail)

getDirID :: Directory -> DirID
getDirID (Directory dirId _) = dirId

getResName :: Resource -> String
getResName (File f) = f
getResName (Dir (Directory Root _)) = "/"
getResName (Dir (Directory (NonRoot res) _)) = res

getDirById :: DirCont -> DirID -> Maybe Directory
getDirById [] _ = Nothing
getDirById ((File _) : tail) dirId = getDirById tail dirId
getDirById (Dir dir@(Directory headId dirContent) : tail) dirId
    | dirId == headId = Just dir
    | otherwise       = getDirById tail dirId


-- | Miscellaneous functions.
mapInit :: (a -> a) -> [a] -> [a]
mapInit f [] = []
mapInit f [x] = [x]
mapInit f (x : xs) = f x : mapInit f xs
