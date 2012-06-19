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
import Numeric
import System.IO
import System.IO.HVFS
import System.Environment
import System.FilePath
import System.Directory
import System.Path
import System.Posix.Files

import qualified Codec.Compression.GZip as GZip
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB

type Package = Paragraph

components = ["main"]
arches     = ["amd64", "i386", "source"]

main = do
    (mirror:suites) <- getArgs
    forM_ suites $ putSuite mirror

putSuite :: String -> String -> IO ()
putSuite m s = do
    forM_ components $ \c -> do
        forM_ arches $ \a -> do
            putArch m s c a

putArch m s c a = do
    let filename = m </> "dists" </> s </> c </> archIndex a
    parseResult <- parseControl filename `liftM` readZipped filename
    case parseResult of
        Left err -> putErr "Parse error" err
        Right packages -> do
            putStr $ showPackages s c a packages
            hFlush stdout
            checkPackages m s c a packages

showPackages s c a control =
    unlines . map showPackage . sortBy (compare `on` pkgName) . unControl $ control
  where showPackage p = printf "%s %s %s %s %s" s c a (pkgName p) (pkgVersion p)

archIndex a = case a of
    "source" ->              a </> "Sources.gz"
    _        -> "binary-" ++ a </> "Packages.gz"

checkPackages :: String -> String -> String -> String -> Control -> IO ()
checkPackages m s c a control =
    forM_ (unControl control) (checker m s c a)
  where checker = case a of
            "source" -> checkSourcePackage
            _        -> checkBinaryPackage

checkBinaryPackage :: String -> String -> String -> String -> Package -> IO ()
checkBinaryPackage m s c a p = do
    let pnm = pkgField "Package"  p
        pfn = pkgField "Filename" p
        psz = pkgField "Size"     p
        pmd = pkgField "MD5sum"   p
    checkPackageFile s c a pnm (m </> pfn) (read psz) pmd

checkSourcePackage :: String -> String -> String -> String -> Package -> IO ()
checkSourcePackage m s c a p = do
    let pnm = pkgField "Package"   p
        pdr = pkgField "Directory" p
        pfs = pkgField "Files"     p
    forM_ (filter (not . null) . lines $ pfs) $ \l -> do
        let [_, pmd, psz, pfn] = split ' ' l
        checkPackageFile s c a pnm (m </> pdr </> pfn) (read psz) pmd

checkPackageFile :: String -> String -> String -> String -> FilePath -> Integer -> String -> IO ()
checkPackageFile s c a pnm fnm psz pmd = do
    exists <- doesFileExist fnm
    if not exists
        then hPutStrLn stderr $ printf "%s/%s/%s/%s/%s: File missing %s" s c a pnm (takeFileName fnm) fnm
        else do
            fsz <- (toInteger . fileSize) `liftM` getFileStatus fnm
            if fsz /= psz
                then hPutStrLn stderr $ printf "%s/%s/%s/%s/%s: Size mismatch: %d instead of %d" s c a pnm (takeFileName fnm) fsz psz
                else do
                    fmd <- md5 fnm
                    if fmd /= pmd
                        then hPutStrLn stderr $ printf "%s/%s/%s/%s/%s: Checksum mismatch: %s instead of %s" s c a pnm (takeFileName fnm) fmd pmd
                        else return ()

pkgName :: Package -> String
pkgName = maybe "Unnamed" B.unpack . fieldValue "Package"

pkgVersion :: Package -> String
pkgVersion = maybe "Unversioned" B.unpack . fieldValue "Version"

pkgField :: String -> Package -> String
pkgField f p = case fieldValue f p of
    Nothing -> error $ "No field " ++ f ++ " in package " ++ pkgName p
    Just v -> B.unpack v

readZipped :: String -> IO B.ByteString
readZipped filename = decompress `fmap` LB.readFile filename
  where decompress' = case takeExtension filename of
            ".gz" -> GZip.decompress
            _     -> id
        decompress = B.concat . LB.toChunks . decompress'

putErr :: String -> ParseError -> IO ()
putErr msg e = hPutStrLn stderr $ msg ++ ": " ++ show e

md5 :: FilePath -> IO String
md5 fp = do
    h <- MD5.hashlazy `liftM` LB.readFile fp
    return $ showHexBytes h ""

showHexBytes :: B.ByteString -> String -> String
showHexBytes bs s = foldr showHexByte s (B.unpack bs)
showHexByte b = tail . showHex (0x100 + fromEnum b)

split :: Eq a => a -> [a] -> [[a]]
split c [] = []
split c s  = h : split c (drop 1 t)
  where (h, t) = break (== c) s

final :: Integral n => n -> [a] -> [a]
final n xs = drop (length xs - fromIntegral n) xs
