module Main where

import Debian.Control.ByteString
import Debian.Relation
import Text.ParserCombinators.Parsec.Error
import Text.Printf
import Data.Ord
import Data.List
import Data.Maybe
import Data.Either
import Data.Function
import Numeric
import Control.Monad
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
type Index = Control

components = ["main"]
arches     = ["amd64", "i386", "source"]

main = do
    (mirror:suites) <- getArgs
    indexes <- forM [(s, c, a) | s <- suites, c <- components, a <- arches]
                  (getIndex mirror)
    forM_ indexes (putIndex mirror)
    forM_ indexes (checkIndex mirror)
    checkDups indexes

getIndex :: String -> (String, String, String) -> IO (String, String, String, Index)
getIndex m (s, c, a) = do
    let filename = m </> "dists" </> s </> c </> archIndex a
        onError err = do putErr "Parse error" err
                         return $ Control []
    result <- parseControl filename `liftM` readZipped filename
    index <- either onError return result
    return (s, c, a, index)
  where
    archIndex a = case a of
        "source" ->              a </> "Sources.gz"
        _        -> "binary-" ++ a </> "Packages.gz"

putIndex :: String -> (String, String, String, Index) -> IO ()
putIndex _ (s, c, a, ix) =
    putStr $ unlines . map showPackage . sortBy (comparing pkgName) . unControl $ ix
  where
    showPackage p = printf "%s %s %s %s %s" s c a (pkgName p) (pkgVersion p)

checkIndex :: String -> (String, String, String, Index) -> IO ()
checkIndex m (s, c, a, control) =
    forM_ (unControl control) (checker m (s, c, a))
  where
    checker = case a of
        "source" -> checkSourcePackage
        _        -> checkBinaryPackage

checkBinaryPackage :: String -> (String, String, String) -> Package -> IO ()
checkBinaryPackage m (s, c, a) p = do
    let pnm = pkgField "Package"  p
        pfn = pkgField "Filename" p
        psz = pkgField "Size"     p
        pmd = pkgField "MD5sum"   p
    checkPackageFile (s, c, a) pnm (m </> pfn) (read psz) pmd

checkSourcePackage :: String -> (String, String, String) -> Package -> IO ()
checkSourcePackage m (s, c, a) p = do
    let pnm = pkgField "Package"   p
        pdr = pkgField "Directory" p
        pfs = pkgField "Files"     p
    forM_ (filter (not . null) . lines $ pfs) $ \l -> do
        let [_, pmd, psz, pfn] = split ' ' l
        checkPackageFile (s, c, a) pnm (m </> pdr </> pfn) (read psz) pmd

checkPackageFile :: (String, String, String) -> String -> FilePath -> Integer -> String -> IO ()
checkPackageFile (s, c, a) pnm fnm psz pmd = do
    exists <- doesFileExist fnm
    if not exists
        then putErr $ printf "File missing %s" fnm
        else do
            fsz <- (toInteger . fileSize) `liftM` getFileStatus fnm
            if fsz /= psz
                then putErr $ printf "Size mismatch: %d instead of %d" fsz psz
                else do
                    fmd <- md5 fnm
                    if fmd /= pmd
                        then putErr $ printf "Checksum mismatch: %s instead of %s" fmd pmd
                        else return ()
  where
    putErr :: String -> IO ()
    putErr msg = hPutStrLn stderr $ printf "%s/%s/%s/%s/%s: %s" s c a pnm (takeFileName fnm) msg

checkDups :: [(String, String, String, Index)] -> IO ()
checkDups indexes =
    let pkgInst (s, c, a) p = ((pkgName p, pkgVersion p, a), (s, c))
        ixInsts (s, c, a, ix) = map (pkgInst (s, c, a)) . unControl $ ix
        allInsts = sortBy (comparing fst) . concatMap ixInsts $ indexes
        dupInsts = filter ((>1) . length) . groupBy ((==) `on` fst) $ allInsts
        problems = map (\grp -> (fst . head $ grp, map snd grp)) dupInsts
    in
        forM_ problems $ \((n, v, a), insts) -> do
            putStrLn $ printf "%s %s %s" n v a
            forM_ insts $ \(s, c) -> do
                putStrLn $ printf "  %s %s" s c

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
