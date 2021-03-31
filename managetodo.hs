import System.Environment(getArgs)
import System.Directory  (doesFileExist, renameFile)
import System.IO         (readFile, writeFile, openTempFile, hPutStr, hClose)
import Control.Monad     (when)

type Index    = Int
type Filename = String
type IntStr   = String -- representing an int

insertAt :: Index -> a -> [a] -> [a]
insertAt index value list = let (left, right) = splitAt index list
                            in  left ++ value:right

deleteAt :: Index -> [a] -> [a]
deleteAt index list = let (left, (_:right)) = splitAt index list
                      in  left ++ right

enumerate :: [a] -> [(IntStr, a)]
enumerate = zip (map show [0..])

writeTempFile :: String -> IO (String)
writeTempFile text = do
    (tempname, handle) <- openTempFile "." "temp"
    hPutStr handle text
    hClose handle
    return tempname

addToDo :: Filename -> Index -> String -> IO (String)
addToDo filename index item = do
    fileContents <- readFile filename
    let newContents = unlines . (insertAt index item) . lines $ fileContents
    writeTempFile newContents

removeToDo :: Filename -> Index -> IO (String)
removeToDo filename index = do
    fileContents <- readFile filename
    let newContents = unlines . (deleteAt index) . lines $ fileContents
    writeTempFile newContents

replaceToDo :: Filename -> Index -> String -> IO (String)
replaceToDo filename index item = do
    fileContents <- readFile filename
    let newContents = unlines . (insertAt index item) . (deleteAt index) . lines $ fileContents
    writeTempFile newContents

viewToDo :: Filename -> IO (String)
viewToDo filename = do
    fileContents <- readFile filename
    let prettify =  unlines . map (\(a,b) -> a ++ " -> " ++ b) . enumerate . lines

    putStr $ prettify fileContents
    return ""

main = do
    (action:fileName:values) <- getArgs
    fileExists <- doesFileExist fileName

    if not fileExists then putStrLn "File does not exist"
                      else do
         newFile <- case action of "view"    -> (   viewToDo fileName                                   )
                                   "add"     -> (    addToDo fileName (read $ head values) (last values))
                                   "replace" -> (replaceToDo fileName (read $ head values) (last values))
                                   "remove"  -> ( removeToDo fileName (read $ head values)              )

         when (not $ null newFile) $ do
             renameFile newFile fileName
