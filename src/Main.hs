module Main where

import VSFS

import Control.Monad.RWS.Lazy

import Data.Char (toLower)
import Data.List.Split (splitOn)

import System.Environment (getArgs)
import System.Console.Haskeline


---
main :: IO ()
main = do
    {- Here I should parse the arguments, but none are defined at the moment. -}
    args <- getArgs
    (keys, values) <- parseArgs args
    putStrLn "Welcome to the VSFS file system manager."
    putStrLn "Type :h for usage information.\n"

    runInputT defaultSettings (loopMan initMan)

    where 
        parseArgs :: [String] -> IO ([String],[String])
        parseArgs _ = return ([],[])

---


---
loopMan :: VSFSMan -> InputT IO ()
loopMan manager = do
    unitInput <- getInputLine "$ "
    case unitInput of
        Nothing -> return ()
        Just inputRaw -> 
            let input = map toLower inputRaw in 
            if input == ":h"
            then do
                printManUsageMenu
                loopMan manager
            else
                if elem (map toLower input) quitSet
                then return ()
                else 
                    case runManCommand input manager of
                        Left err -> do
                            outputStrLn err
                            loopMan manager
                        Right (L listing) -> do
                            lift listing
                            loopMan manager 
                        Right (V newMan) -> loopMan newMan
                        Right (S (leftMan, rightMan)) -> 
                            let fsName = (fst . head) rightMan in
                            let fs = (snd . head) rightMan in
                            let rest = tail rightMan in do
                                outputStrLn $ 
                                    "Switching to " ++ fsName ++ " file system."
                                outputStrLn "Type :h for usage information.\n"
                                updatedFS <- loopFS fs
                                outputStrLn $
                                    "Switching back to the manager level."
                                outputStrLn "Type :h for usage information.\n"
                                loopMan $ leftMan ++ [(fsName, updatedFS)] ++ rest


data ResultAlternative = V VSFSMan | S (VSFSMan, VSFSMan) | L (IO ())
type RunManCommandError = String
type RunManCommandResult = Either RunManCommandError ResultAlternative

runManCommand :: String -> VSFSMan -> RunManCommandResult
runManCommand input manager =  
    case parseManCommand input of
        Right List -> Right $ L $ list manager
        Right (Init fsName) -> 
            case manager `initialize` fsName of
                Left err -> Left err
                Right newMan -> Right $ V newMan
        Right (Delete fsName) -> 
            case manager `delete` fsName of
                Left err -> Left err
                Right newMan -> Right $ V newMan
        Right (Switch fsName) -> 
            case manager `switch` fsName of
                Left err -> Left err
                Right switching -> Right $ S switching


parseManCommand :: String -> Either String ManCommand
parseManCommand input =
    let (cmd:args) = splitOn " " input in
        case cmd of
            "list" -> 
                if length args == 0
                then Right $ List
                else Left $ numberArgsError cmd 0
            "init" -> 
                if length args == 1
                then Right $ Init (head args)
                else Left $ numberArgsError cmd 1
            "switch" -> 
                if length args == 1
                then Right $ Switch (head args)
                else Left $ numberArgsError cmd 1
            "delete" ->
                if length args == 1
                then Right $ Delete (head args)
                else Left $ numberArgsError cmd 1
            _ -> Left "Wrong command."


numberArgsError :: String -> Int -> String
numberArgsError str num = str ++ " takes exactly " ++ show num ++ " argument(s)."
---


---
loopFS :: FS -> InputT IO FS
loopFS fileSystem = do
    unitInput <- getInputLine "$ "
    case unitInput of
        Nothing -> return fileSystem
        Just input -> 
            if (map toLower input) == ":h"
            then do
                printFSUsageMenu
                loopFS fileSystem
            else
                if elem (map toLower input) quitSet
                then return fileSystem
                else
                    let (newFileSystem, msg) = runSession (runCommand input) fileSystem in
                        do
                            when (msg /= "") $ outputStrLn msg
                            loopFS newFileSystem


runCommand :: String -> Session ()
runCommand input = 
    case parseCommand input of
        Right (AddDir dirId) -> addDirMonadic (Directory (NonRoot dirId) [])
        Right (AddFile fileId) -> addFileMonadic (File fileId)
        Right (RmFile fileId) -> rmFileMonadic fileId
        Right (Cd dirId) -> cdMonadic (NonRoot dirId)
        Right CdUp -> cdUpMonadic
        Right Pwd -> pwdMonadic
        Right Ls -> lsMonadic
        Right (Find fileId) -> findMonadic fileId
        Left errMsg -> tell errMsg


parseCommand :: String -> Either String Command
parseCommand input = 
    let (cmd:args) = splitOn " " input in
        case cmd of
            "addDir" -> 
                if length args == 1
                then Right $ AddDir (head args)
                else Left $ numberArgsError cmd 1
            "addFile" -> 
                if length args == 1
                then Right $ AddFile (head args)
                else Left $ numberArgsError cmd 1
            "rmFile" -> 
                if length args == 1
                then Right $ RmFile (head args)
                else Left $ numberArgsError cmd 1
            "cd" ->
                if length args == 1
                then Right $ Cd (head args)
                else Left $ numberArgsError cmd 1
            "cdup" -> 
                if length args == 0
                then Right CdUp
                else Left $ numberArgsError cmd 0
            "pwd" -> 
                if length args == 0
                then Right Pwd
                else Left $ numberArgsError cmd 0
            "ls" ->
                if length args == 0
                then Right Ls
                else Left $ numberArgsError cmd 0
            "find" -> 
                if length args == 1
                then Right $ Find (head args)
                else Left $ numberArgsError cmd 1
            _ -> Left "Wrong command."
---


---
printManUsageMenu :: InputT IO ()
printManUsageMenu = do
    outputStrLn ""
    outputStrLn "MANAGER USAGE:"
    outputStrLn "list\t\t\t\tLists the file systems."
    outputStrLn "init fileSystemName\t\tInitializes a file system called fileSystemName."
    outputStrLn "switch fileSystemName\t\tGo to file system called fileSystemName."
    outputStrLn "delete fileSystemName\t\tDeletes the file system called fileSystemName."
    outputStrLn ""


printFSUsageMenu :: InputT IO ()
printFSUsageMenu = do
    outputStrLn ""
    outputStrLn "FILE SYSTEMS USAGE:"
    outputStrLn "fsman\t\t\t\tReturn to the file systems manager."
    outputStrLn "pwd\t\t\t\tPrints the current directory of the file system."
    outputStrLn "ls\t\t\t\tLists the content of the current directory (the \"(d)\" prefix indicates a directory)."
    outputStrLn $ "addFile fileName\t\tAdds a file called fileName to the current directory." ++
        " It prints an error message if the file already exists."
    outputStrLn $ "addDir directoryName\t\tAdds a directory called directoryName to the current directory." ++
        " It prints an error message if the directory already exists."
    outputStrLn $ "cd directoryName\t\tEnter the directory called directoryName." ++ 
        " It prints an error message if the directory does not exist."
    outputStrLn $ "cdup\t\t\t\tChanges directory to the immediate parent of the current directory." ++ 
        " It prints an error message if the current directory is the root of the file system."
    outputStrLn $ "find fileName\t\t\tPrints the complete path of each occurrence of the fileName under the current directory."
    outputStrLn $ "rmFile fileName\t\t\tRemoves the file located at path fileName." ++ 
        "It prints an error message if the file cannot be found."
    outputStrLn ""
---


---
quitSet = ["quit", ":q"]
---
