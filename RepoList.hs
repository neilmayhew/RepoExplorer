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
    forM_ suites $ putSuite mirror

putSuite :: String -> String -> IO ()
putSuite m s = do
    release' <- parseControlFromFile $ m </> s </> "Release"
    let release = head . unControl . fromRight $ release'
        components = maybe [] (words . B.unpack) . fieldValue "Components"    $ release
        arches'    = maybe [] (words . B.unpack) . fieldValue "Architectures" $ release
        arches     = sort arches' ++ ["source"]
    forM_ components $ \c -> do
        forM_ arches $ \a -> do
            putArch m s c a

putArch m s c a = do
    let filename = m </> s </> c </> archIndex a
    parseResult <- parseControl filename `liftM` readZipped filename
    either (putErr "Parse error") (putStr . showPackages s c a) parseResult

showPackages s c a control =
    unlines . map showPackage . sortBy (compare `on` pkgName) . unControl $ control
  where showPackage p = printf "%s %s %s %s %s" s c a (pkgName p) (pkgVersion p)

archIndex a = case a of
    "source" ->              a </> "Sources.gz"
    _        -> "binary-" ++ a </> "Packages.gz"

pkgName :: Package -> String
pkgName = maybe "Unnamed" B.unpack . fieldValue "Package"

pkgVersion :: Package -> String
pkgVersion = maybe "Unversioned" B.unpack . fieldValue "Version"

readZipped :: String -> IO B.ByteString
readZipped filename = decompress `fmap` LB.readFile filename
  where decompress' = case takeExtension filename of
            ".gz" -> GZip.decompress
            _     -> id
        decompress = B.concat . LB.toChunks . decompress'

putErr :: String -> ParseError -> IO ()
putErr msg e = hPutStrLn stderr $ msg ++ ": " ++ show e

final n xs = drop (length xs - n) xs
