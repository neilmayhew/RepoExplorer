module Main where

import Debian.Control.ByteString
import Debian.Relation
import Text.ParserCombinators.Parsec.Error
import Text.Printf
import Data.List
import Data.Maybe
import Data.Function
import Control.Monad
import System.IO
import System.IO.HVFS
import System.Environment
import System.FilePath
import System.Directory
import System.Path

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB

type Package = Paragraph

main = do
    roots <- getArgs
    files <- concat `liftM` mapM (recurseDir SystemFS) roots
    let lists = sortDirs . filter isList $ files
    forM_ lists $ \f -> readZipped f >>= showPackages f
  where
    sortDirs = sortBy (compare `on` splitDirectories)
    isList = (`elem` ["Packages.gz", "Sources.gz"]) . takeFileName

readZipped filename = decompress `fmap` LB.readFile filename
  where decompress' = case takeExtension filename of
            ".gz" -> GZip.decompress
            _     -> id
        decompress = B.concat . LB.toChunks . decompress'

showPackages filename = either (putErr "Parse error") (putStr . showControl filename) . parseControl filename

showControl filename control = 
    unlines . map showPackage . sortBy (compare `on` pkgName) . unControl $ control
  where [dist, section, arch] = final 3 . splitDirectories . takeDirectory $ filename
        showPackage p = printf "%s %s %s %s %s" dist section arch (pkgName p) (pkgVersion p)

putErr :: String -> ParseError -> IO ()
putErr msg e = hPutStrLn stderr $ msg ++ ": " ++ show e

pkgName :: Package -> String
pkgName = maybe "Unnamed" B.unpack . fieldValue "Package"

pkgVersion :: Package -> String
pkgVersion = maybe "Unversioned" B.unpack . fieldValue "Version"

final n xs = drop (length xs - n) xs
