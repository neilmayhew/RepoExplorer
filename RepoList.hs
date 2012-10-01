{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Debian.Control.ByteString
import Debian.Relation
import Debian.Version
import System.Console.CmdArgs.Implicit
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

import Network.Curl.Download.Lazy

type Package = Paragraph
type Index = Control

defComponents = ["main"]
defArches     = ["amd64", "i386", "source"]

data Options = Options
    { optCheckSums :: Bool
    , optCheckDups :: Bool
    , optComponents :: [String]
    , optArches     :: [String]
    , argMirror :: String
    , argSuites :: [String] }
    deriving (Show, Data, Typeable)

options = Options
    { optCheckSums = False &= name "check-sums" &= name "s" &= explicit &= help "Check package sums" &= groupname "Options"
    , optCheckDups = False &= name "check-dups" &= name "d" &= explicit &= help "Check package duplicates"
    , optComponents = [] &= typ "NAMES" &= name "components" &= name "c" &= explicit &= help "Components to list"
    , optArches     = [] &= typ "NAMES" &= name "arches"     &= name "a" &= explicit &= help "Architectures to list"
    , argMirror = "" &= argPos 0 &= typ "MIRROR"
    , argSuites = [] &= args     &= typ "SUITE" }
        &= program "RepoList"
        &= summary "List and optionally check repository contents"
        &= versionArg [summary "RepoList v0.5"]

main = do
    args <- cmdArgs options
    let mirror = argMirror args
        suites = argSuites args
        components = concatMap words $ optComponents args `withDefault` defComponents
        arches     = concatMap words $ optArches     args `withDefault` defArches
        doCheckSums = optCheckSums args
        doCheckDups = optCheckDups args
        withDefault l d = if null l then d else l
    when (null suites) $ error "Must specify suites"
    indexes <- forM [(s, c, a) | s <- suites, c <- components, a <- arches]
                  (getIndex mirror)
    forM_ indexes (putIndex mirror)
    when doCheckSums $ forM_ indexes (checkIndex mirror)
    when doCheckDups $ checkDups indexes

getIndex :: String -> (String, String, String) -> IO (String, String, String, Index)
getIndex m (s, c, a) = do
    let location = m </> "dists" </> s </> c </> archIndex a
        onError err = do putErr "Parse error" err
                         return $ Control []
    result <- parseControl location `liftM` readZipped location
    index <- either onError return result
    return (s, c, a, index)
  where
    archIndex a = case a of
        "source" ->              a </> "Sources.gz"
        _        -> "binary-" ++ a </> "Packages.gz"

putIndex :: String -> (String, String, String, Index) -> IO ()
putIndex _ (s, c, a, ix) =
    putStr $ unlines . map showPackage . sortBy pkgCompare . unControl $ ix
  where
    showPackage p = printf "%s %s %s %s %s" s c a (pkgName p) (show $ pkgVersion p)

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
    let pkgInst (s, c, a) p = ((pkgName p, pkgVersion p, a), (s, c, pkgHash p))
        ixInsts (s, c, a, ix) = map (pkgInst (s, c, a)) . unControl $ ix
        allInsts = combineAssocs . concatMap ixInsts
        uniqueInsts = mapSnds (nubBy sameHash) . allInsts
        dupInsts = filter ((>1) . length . snd) . uniqueInsts
        sameHash = (==) `on` thd
        thd (_, _, x) = x
    in
        forM_ (dupInsts indexes) $ \((n, v, a), insts) -> do
            hPutStrLn stderr $ printf "%s %s %s" n (show v) a
            forM_ insts $ \(s, c, h) -> do
                hPutStrLn stderr $ printf "%12s %-12s %s" s c h

groupSortBy :: (Eq b, Ord b) => (a -> b) -> [a] -> [[a]]
groupSortBy field = groupBy ((==) `on` field) . sortBy (comparing field)

combineAssocs :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
combineAssocs = map combine . groupSortBy fst
  where combine g = (fst . head $ g, map snd g)

pkgCompare :: Package -> Package -> Ordering
pkgCompare p1 p2 =
    (compare `on` pkgName) p1 p2
        ||| (compare `on` pkgVersion) p1 p2
        ||| (compare `on` pkgArch) p1 p2
  where
    (|||) a b = if a /= EQ then a else b

pkgName :: Package -> String
pkgName = maybe "Unnamed" B.unpack . fieldValue "Package"

pkgVersion :: Package -> DebianVersion
pkgVersion = parseDebianVersion . maybe "Unversioned" B.unpack . fieldValue "Version"

pkgArch :: Package -> String
pkgArch = maybe "None" B.unpack . fieldValue "Architecture"

pkgHash :: Package -> String
pkgHash p = if isSrc p
    then dsc !! 1
    else pkgField "MD5sum" p
  where
    isSrc = isJust . fieldValue "Format"
    infos = map (split ' ') . filter (not . null) . lines . pkgField "Files" $ p
    dsc = fromJust . find ((== ".dsc") . takeExtension . (!! 3)) $ infos

pkgFile :: Package -> String
pkgFile p = if isSrc p
    then dsc !! 3
    else pkgField "Filename" p
  where
    isSrc = isJust . fieldValue "Format"
    infos = map (split ' ') . filter (not . null) . lines . pkgField "Files" $ p
    dsc = fromJust . find ((== ".dsc") . takeExtension . (!! 3)) $ infos

pkgField :: String -> Package -> String
pkgField f p = case fieldValue f p of
    Nothing -> error $ "No field " ++ f ++ " in package " ++ pkgName p
    Just v -> B.unpack v

readZipped :: String -> IO B.ByteString
readZipped location = decompress `liftM` readLazyURI location
  where decompress = B.concat . LB.toChunks . GZip.decompress

readLazyURI :: String -> IO LB.ByteString
readLazyURI url = either err return =<< openLazyURI url
  where err msg = error $ msg ++ " (" ++ url ++ ")"

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

mapFsts :: (a -> b) -> [(a, c)] -> [(b, c)]
mapFsts f = map (\(x, y) -> (f x, y))

mapSnds :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnds f = map (\(x, y) -> (x, f y))
