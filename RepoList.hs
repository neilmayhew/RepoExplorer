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
import Data.Either.Utils
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

import Network.Curl.Download
import Network.Curl.Download.Lazy

type Package     = Paragraph
type PackageList = Control

data Index = Index
    { ixSuite :: String
    , ixComp  :: String
    , ixArch  :: String
    , ixList  :: PackageList }

data Options = Options
    { argMirror     :: String
    , argSuites     :: [String]
    , optComponents :: String
    , optArches     :: String
    , optCheckSums  :: Bool
    , optCheckDups  :: Bool
    } deriving (Show, Data, Typeable)

options = Options
    { argMirror     = ""    &= argPos 0                       &= typ "MIRROR"
    , argSuites     = []    &= args                           &= typ "SUITES"
    , optComponents = ""    &= name "components"  &= name "c" &= typ "NAMES" &= explicit &= help "Components to include" &= groupname "Options"
    , optArches     = ""    &= name "arches"      &= name "a" &= typ "NAMES" &= explicit &= help "Architectures to include"
    , optCheckSums  = False &= name "check-sums"  &= name "s"                &= explicit &= help "Check package sums"
    , optCheckDups  = False &= name "check-dups"  &= name "d"                &= explicit &= help "Check package duplicates"
    }   &= program "RepoList"
        &= summary "List and optionally check repository contents"
        &= versionArg [summary "RepoList v0.5"]
        &= details
        [ "If components and architectures aren't specified, they are taken from the"
        , "Release file for the suite"
        , ""
        , "Examples:"
        , "    RepoList http://ppa.launchpad.net/gnome3-team/gnome3/ubuntu precise quantal"
        , "    RepoList -c 'main non-free' -a mips http://www.deb-multimedia.org wheezy" ]

main = do
    args <- cmdArgs options

    let mirror     = argMirror args
        suites     = argSuites args
        components = words $ optComponents args
        arches     = words $ optArches     args

    when (null suites) $ error "Must specify suites"

    indexes <- getRepo mirror suites components arches

    forM_ indexes (putIndex mirror)

    when (optCheckSums args) $ forM_ indexes (checkIndex mirror)
    when (optCheckDups args) $ checkDups indexes

getRepo :: String -> [String] -> [String] -> [String] -> IO [Index]
getRepo mirror suites components arches =
    concat `liftM` forM (sort suites) (\s -> getSuite mirror s components arches)

getSuite :: String -> String -> [String] -> [String] -> IO [Index]
getSuite mirror suite uComponents uArches = do
    (rComponents, rArches) <- if null uComponents || null uArches
        then getReleaseParts mirror suite
        else return ([], [])
    let components = if null uComponents then rComponents else uComponents
        arches     = if null uArches     then rArches     else uArches
    forM [(suite, c, a) | c <- sort components, a <- sort arches] $ getIndex mirror
  where
    getReleaseParts m s = do
        let location = m </> "dists" </> s </> "Release"
        releaseData <- readURI location
        let release = head . unControl . fromRight . parseControl location $ releaseData
            components = words $ fieldValue' "Components"    release
            arches     = words $ fieldValue' "Architectures" release
        return (components, arches ++ ["source"])
      where
        fieldValue' name = maybe "" B.unpack . fieldValue name

getIndex :: String -> (String, String, String) -> IO Index
getIndex m (s, c, a) = do
    let location = m </> "dists" </> s </> c </> archIndex a
        onError err = do putErr "Parse error" err
                         return $ Control []
    result <- parseControl location `liftM` readZipped location
    list <- either onError return result
    return $ Index s c a list
  where
    archIndex a = case a of
        "source" ->              a </> "Sources.gz"
        _        -> "binary-" ++ a </> "Packages.gz"

putIndex :: String -> Index -> IO ()
putIndex _ (Index s c a l) =
    putStr $ unlines . map showPackage . sortBy pkgCompare . unControl $ l
  where
    showPackage p = printf "%s %s %s %s %s" s c a (pkgName p) (show $ pkgVersion p)

checkIndex :: String -> Index -> IO ()
checkIndex m (Index s c a l) =
    forM_ (unControl l) (checker m (s, c, a))
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

checkDups :: [Index] -> IO ()
checkDups indexes =
    let pkgInst (s, c, a) p = ((pkgName p, pkgVersion p, a), (s, c, pkgHash p))
        ixInsts (Index s c a l) = map (pkgInst (s, c, a)) . unControl $ l
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

readURI :: String -> IO B.ByteString
readURI url = either err return =<< openURI url
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
