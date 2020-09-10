module Main where

import CommonMark

import Text.Parsec

srcFile = "test/commonmark1.md"
destFileHtml = "test/result.html"
destFileCm = "test/result.md"

main :: IO ()
main = mainProgram -- do
    -- text <- readFile srcFile
    -- -- let text = ""
    -- let r = runParser pBlocks initState "" text
    -- print r
    -- case r of
    --     Right doc -> (do writeFile destFileHtml $ concatMap toHtml doc
    --                      writeFile destFileCm $ concatMap (toCmBlock 0) doc)
    --     _         -> error "parsing failed"
    -- return ()

mainProgram :: IO ()
mainProgram = do
    putStrLn "Enter file path:"
    fp <- getLine
    res <- pDocument fp
    case res of
        Right doc -> readCommand doc
        _         -> error "parsing failed"

readCommand :: Block -> IO ()
readCommand doc = do
    putStrLn "Enter a command or help for list of commands:"
    cmd <- getLine
    case cmd of
        "exit" -> return()
        "help" -> (do readCommand doc)
        _      -> (do parseCommand doc cmd
                      readCommand doc)
                
parseCommand :: Block -> String -> IO ()
parseCommand doc c = do
    let args = words c
    case args of
        "save":args' -> saveDocument doc args'
        _            -> putStrLn "unknown command"

saveDocument :: Block -> [String] -> IO ()
saveDocument doc [t,dest] = do
    let dw = getDocWriter t
    case dw of
        Just wf -> writeFile dest $ wf bs
        Nothing -> putStrLn "unknown format. use html or cm."
    where
        (Document _ bs) = doc
saveDocument doc _ = do
    putStrLn "specify format and save location"
    
getDocWriter :: String -> Maybe (Blocks -> String)
getDocWriter "html" = Just (concatMap toHtml)
getDocWriter "cm"   = Just (concatMap (toCmBlock 0))
getDocWrtier _      = Nothing

