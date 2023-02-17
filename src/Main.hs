{-# LANGUAGE LambdaCase #-}

module Main where

import VSFS

import Control.Monad.RWS.Lazy
import Data.Char (toLower)
import Data.List.Split (splitOn)
import System.Environment (getArgs)
import System.Console.Haskeline

import Types


main :: IO ()
main = do
    -- args <- getArgs
    -- (keys, values) <- parseArgs args
    putStrLn "Welcome to the VSFS file system manager."
    putStrLn "Type :h for usage information.\n"
    runInputT defaultSettings (loopMan initMan)
    -- where
    --   parseArgs :: [String] -> IO ([String],[String])
    --   parseArgs _ = return ([],[])

loopMan :: VSFSMan -> InputT IO ()
loopMan man = do
    unitInput <- getInputLine "$ "
    case unitInput of
        Nothing -> return ()
        Just input
            | elem input [":q", ":Q"] -> return ()
            | elem input [":h", ":H"] -> printManUsageMenu >> loopMan man
            | otherwise               -> runManCommand input >>= loopMan
  where
    printManUsageMenu :: InputT IO ()
    printManUsageMenu = outputStrLn $ concat
        [ "\nMANAGER USAGE:\n"
        , "list\t\tLists the file systems.\n"
        , "init fs\t\tInitializes a file system called fs.\n"
        , "switch fs\tGo to file system fs.\n"
        , "delete fs\tDeletes file system fs\n\n"
        ]

    runManCommand :: String -> InputT IO VSFSMan
    runManCommand input = case runManCommand' input of
        Right (L l) -> mapM_ (lift . putStrLn) l >> return man
        Right (V newMan) -> return newMan
        Right (S (leftMan, [])) -> do
            outputStrLn "File system could not be created."
            return man
        Right (S (leftMan, (fsName, fs) : rest)) -> do
            outputStrLn $
                "Switching to " ++ fsName ++ " file system.\n"
                    ++ "Type :h for usage information.\n"
            updatedFS <- loopFS fs
            outputStrLn "Switching back to the manager level."
            outputStrLn "Type :h for usage information.\n"
            return $ leftMan ++ [(fsName, updatedFS)] ++ rest
        Left err -> outputStrLn err >> return man

    runManCommand' :: String -> Either String ResultAlternative
    runManCommand' input = parseManCommand input >>= \case
        List          -> return $ L $ list man
        Init fsName   -> fsName `initialize` man  >>= return . V
        Delete fsName -> fsName `delete` man >>= return . V
        Switch fsName -> fsName `switch` man >>= return . S

    parseManCommand :: String -> Either String ManCommand
    parseManCommand input =
        let (cmd:args) = splitOn " " input
        in case cmd of
            "list"   | length args == 0 -> Right List
                     | otherwise        -> Left $ numberArgsError cmd 0
            "init"   | length args == 1 -> Right $ Init $ head args
                     | otherwise        -> Left $ numberArgsError cmd 1
            "switch" | length args == 1 -> Right $ Switch $ head args
                     | otherwise        -> Left $ numberArgsError cmd 1
            "delete" | length args == 1 -> Right $ Delete $ head args
                     | otherwise        -> Left $ numberArgsError cmd 1
            _                           -> Left "Wrong command."

numberArgsError :: String -> Int -> String
numberArgsError s n = s ++ " takes exactly " ++ show n ++ " argument(s)."

loopFS :: FS -> InputT IO FS
loopFS fs = getInputLine "$ " >>= \case
    Just input | elem input [":h", ":H"] -> printFSUsageMenu >> loopFS fs
               | elem input [":q", ":Q"] -> return fs
               | otherwise ->
                    let (newFS, msg) = runSession (runCommand input) fs
                    in when (msg /= "") (outputStrLn msg) >> loopFS newFS
    Nothing -> return fs

runCommand :: String -> Session
runCommand input =
    case parseCommand input of
        Right (MkDir dirId)    -> mkDirMonadic (Directory (NonRoot dirId) [])
        Right (AddFile fileId) -> addFileMonadic fileId
        Right (RmFile fileId)  -> rmFileMonadic fileId
        Right (Cd dirId)       -> cdMonadic (NonRoot dirId)
        Right CdUp             -> cdUpMonadic
        Right Pwd              -> pwdMonadic
        Right Ls               -> lsMonadic
        Right (Find fileId)    -> findMonadic fileId
        Left errMsg            -> tell errMsg

parseCommand :: String -> Either String Command
parseCommand input =
    let (cmd:args) = splitOn " " input
    in case cmd of
        "mkDir"   | length args == 1 -> Right $ MkDir (head args)
                  | otherwise        -> Left $ numberArgsError cmd 1
        "addFile" | length args == 1 -> Right $ AddFile (head args)
                  | otherwise        -> Left $ numberArgsError cmd 1
        "rmFile"  | length args == 1 -> Right $ RmFile (head args)
                  | otherwise        -> Left $ numberArgsError cmd 1
        "cd"      | length args == 1 -> Right $ Cd (head args)
                  | otherwise        -> Left $ numberArgsError cmd 1
        "cdup"    | length args == 0 -> Right CdUp
                  | otherwise        -> Left $ numberArgsError cmd 0
        "pwd"     | length args == 0 -> Right Pwd
                  | otherwise        -> Left $ numberArgsError cmd 0
        "ls"      | length args == 0 -> Right Ls
                  | otherwise        -> Left $ numberArgsError cmd 0
        "find"    | length args == 1 -> Right $ Find (head args)
                  | otherwise        -> Left $ numberArgsError cmd 1
        _                            -> Left "Wrong command."

printFSUsageMenu :: InputT IO ()
printFSUsageMenu = outputStrLn $ concat
    [ "\nFILE SYSTEMS USAGE:\n"
    , "pwd\t\tPrints the current directory of the file system.\n"
    , "ls\t\tLists the content of the current directory (the \"(d)\" prefix "
    , "indicates a directory).\n"
    , "addFile f\tAdds file f to the current directory.\n"
    , "mkDir d\tAdds directory d within the current directory.\n"
    , "cd d\t\tEnters directory d located in the current directory.\n"
    , "cdup\t\tChanges directory to the immediate parent of the current "
    , "directory.\n"
    , "find f\t\tPrints complete path of each occurrence of file f under the "
    , "current directory.\n"
    , "rmFile p\tRemoves file located at path p, starting from the current "
    , "directory.\n"
    , ":q\t\tReturn to the file systems manager.\n"
    ]
