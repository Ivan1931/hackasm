{-# LANGUAGE QuasiQuotes #-}

import Parser (parseLines)
import Assembler (assemble, assembleBinString)
import Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (when)
import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)
import System.Console.Docopt

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs

  when (args `isPresent` (command "assemble")) $ do
    inputPath <- args `getArgOrExit` (argument "input")
    outputPath <- args `getArgOrExit` (argument "output")
    contents <- TIO.readFile inputPath
    if args `isPresent` (longOption "binary") then
      let
        doAssemble = do
          (_, result) <- parseLines $ T.lines contents
          assemble result
      in case doAssemble of
        Left   err   -> print err
        Right  bytes -> BL.writeFile outputPath bytes
    else
      let
        doAssemble = do
          (_, result) <- parseLines $ T.lines contents
          assembleBinString result
      in case doAssemble of
        Left err -> print err
        Right bitString -> writeFile outputPath $ Prelude.unlines bitString
      

  when (args `isPresent` (command "analyse")) $ do
    inputPath <- args `getArgOrExit` (argument "input")
    contents <- TIO.readFile inputPath
    case parseLines . T.lines $ contents of
      Left  err   -> putStrLn ("Error: " ++ show err)
      Right (consumed, result) -> do
        mapM_ print result
        print consumed
 
