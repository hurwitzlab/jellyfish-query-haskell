module Main where

import Data.Monoid
import Options.Applicative
import System.FilePath.Posix (joinPath, takeBaseName, splitFileName)
import System.FilePath.Find 
import System.Directory
import System.Directory
import System.Process
import Control.Monad
import System.Exit

-- | --------------------------------------------------
data Options = Options {
    inputFile :: String,
    outDir    :: String,
    suffixDir :: String,
    jellyfish :: String,
    kmerSize  :: Int
} deriving (Show)

-- | --------------------------------------------------
runWithOptions :: Options -> IO ()
runWithOptions opts = do
    let inFile = inputFile opts
    let (inFileDir, _) = splitFileName inFile
    let inLoc = joinPath [inFileDir, ((takeBaseName inFile) ++ ".locs")]
    
    exists <- doesFileExist inLoc
    when (not exists) 
        (fallOverAndDie ("Location file " ++ inLoc ++ " does not exist."))

    suffixFiles <- find (depth ==? 0) (extension ==? ".jf") (suffixDir opts)

    when (0 == length suffixFiles) 
        (fallOverAndDie ("No suffix files located in " ++ suffixDir opts))

    let jellyfishBin = jellyfish opts

    jfExists <- doesFileExist jellyfishBin
    when (not jfExists) (fallOverAndDie ("Bad jellyfish: " ++ jellyfishBin))

    forM_ suffixFiles $ (\suffix -> do
        let writeDir = joinPath [outDir opts, takeBaseName suffix]
        dirExists <- doesDirectoryExist writeDir
        when (not dirExists) (createDirectory writeDir)

        jfOut <- readProcess jellyfishBin ["query", "-s", inFile, suffix] []
        putStrLn $ "read " ++ suffix
        )

-- | --------------------------------------------------
fallOverAndDie :: String -> IO a
fallOverAndDie err = do putStrLn err
                        exitWith (ExitFailure 1)

-- | --------------------------------------------------
main :: IO ()
main = execParser opts >>= runWithOptions
  where
    parser = Options <$> strOption
                         ( long "input"
                         <> short 'i'
                         <> metavar "INPUT" )
                     <*> strOption
                            ( long "outdir"
                           <> short 'o'
                           <> value "."
                           <> metavar "OUTDIR" )
                     <*> strOption
                            ( long "suffixdir"
                           <> short 's'
                           <> metavar "SUFFIXDIR" )
                     <*> strOption
                            ( long "jellyfish"
                           <> short 'j'
                           <> metavar "JELLYFISHBIN" )
                     <*> option auto
                            ( long "kmer"
                           <> short 'k'
                           <> value 20
                           <> metavar "KMER_SIZE" )
    opts = info parser mempty
