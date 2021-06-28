module Main where

import VSFS

import Data.Char
import Control.Monad.RWS.Lazy
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

    {- Here I implement the actual functionalities of the program. -}
    runInputT defaultSettings (loop initFS)
    where
        loop :: FS -> InputT IO ()
        loop fileSystem = do
            unitInput <- getInputLine "$ "
            case unitInput of
                Nothing -> return ()
                Just input -> 
                    if (map toLower input) == ":h"
                    then do
                        printUsageMenu
                        loop fileSystem
                    else
                        if elem (map toLower input) quitSet
                        then return ()
                        else
                            let (newFileSystem, msg) = execRWS (runCommand input initFS) () fileSystem in
                                do
                                    outputStrLnNotEmpty msg
                                    loop newFileSystem


        outputStrLnNotEmpty :: String -> InputT IO ()
        outputStrLnNotEmpty msg
            | msg == "" = return ()
            | otherwise = outputStrLn msg
---


---
runCommand :: String -> FS -> Session ()
runCommand input fileSystem = 
    case parseCommand input of
        Left (AddDir dirId) -> addDirMonadic (Directory (NonRoot dirId) [])
        Left (AddFile fileId) -> addFileMonadic (File fileId)
        Left (RmFile fileId) -> rmFileMonadic fileId -- implementation misisng
        Left (Cd dirId) -> cdMonadic (NonRoot dirId)
        Left CdUp -> cdUpMonadic
        Left Pwd -> pwdMonadic
        Left Ls -> lsMonadic
        Left (Find fileId) -> findMonadic fileId
        Right errMsg -> tell errMsg
---


---
parseArgs :: [String] -> IO ([String],[String])
parseArgs _ = return ([],[])
---


---
parseCommand :: String -> Either Command String
parseCommand input = 
    case cmd of
        "addDir" -> 
            if length args == 1
            then Left $ AddDir (head args)
            else Right $ numberArgsError cmd 1
        "addFile" -> 
            if length args == 1
            then Left $ AddFile (head args)
            else Right $ numberArgsError cmd 1
        "rmFile" -> 
            if length args == 1
            then Left $ RmFile (head args)
            else Right $ numberArgsError cmd 1
        "cd" ->
            if length args == 1
            then Left $ Cd (head args)
            else Right $ numberArgsError cmd 1
        "cdup" -> 
            if length args == 0
            then Left CdUp
            else Right $ numberArgsError cmd 0
        "pwd" -> 
            if length args == 0
            then Left Pwd
            else Right $ numberArgsError cmd 0
        "ls" ->
            if length args == 0
            then Left Ls
            else Right $ numberArgsError cmd 0
        "find" -> 
            if length args == 1
            then Left $ Find (head args)
            else Right $ numberArgsError cmd 1
        _ -> Right "Wrong command."
    where
        (cmd:args) = splitOn " " input

        numberArgsError :: String -> Int -> String
        numberArgsError str num = str ++ " takes exactly " ++ show num ++ " argument(s)."
---


---
createCommand :: (String, [String]) -> Command
createCommand (cmd,xs) = AddDir ""
---


---
printUsageMenu :: InputT IO ()
printUsageMenu = do
    outputStrLn ""
    outputStrLn "Usage:\n"
    outputStrLn "pwd\t\t\t\tPrints the current directory of the file system."
    outputStrLn "ls\t\t\t\tLists the content of the current directory (the \"(d)\" indicates directories)."
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