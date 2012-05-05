module Main where

import Debian.Control.ByteString
import Debian.Relation
import Text.ParserCombinators.Parsec.Error
import Text.Printf
import Data.List
import Data.Maybe
import Data.Either.Utils
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
    (mirror:suites) <- getArgs
    forM_ suites $ showSuite mirror

showSuite :: String -> String -> IO ()
showSuite m s = do
    release' <- parseControlFromFile $ m </> s </> "Release"
    let release = head . unControl . fromRight $ release'
    let components = maybe [] (words . B.unpack) . fieldValue "Components"    $ release
        arches     = maybe [] (words . B.unpack) . fieldValue "Architectures" $ release
        indexes = map (\a -> "binary-" ++ a </> "Packages.gz") (sort arches)
                    ++ ["source" </> "Sources.gz"]
    forM_ components $ \c -> do
        forM_ indexes $ \i -> do
            let f = m </> s </> c </> i
            readZipped f >>= showPackages f

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
