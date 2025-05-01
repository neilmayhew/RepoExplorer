{-# LANGUAGE CPP, DeriveDataTypeable #-}

module Main where

import Debian.Control.ByteString
import Debian.Version
import System.Console.CmdArgs.Implicit
import Text.ParserCombinators.Parsec.Error
import Text.Printf
import Data.Ord
import Data.List
    ( (\\)
    , find
    , intercalate
    , isPrefixOf
    , minimumBy
    , nub
    , nubBy
    , sort
    , sortBy
    )
import Data.Maybe
import Data.Function
import Network.Curl.Opts
import Numeric
import Control.Monad
import Control.Arrow
import System.IO
import System.FilePath
import System.Directory
import System.Posix.Files

import qualified Codec.Compression.GZip as GZip
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.List.NonEmpty as NE

import Network.Curl.Download
import Network.Curl.Download.Lazy

#if !MIN_VERSION_base(4,10,0)
fromRight :: b -> Either a b -> b
fromRight d = either (const d) id
#endif

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
    , optCheckVers  :: Bool
    } deriving (Show, Data)

options :: Options
options = Options
    { argMirror     = ""    &= argPos 0                       &= typ "MIRROR"
    , argSuites     = []    &= args                           &= typ "SUITES"
    , optComponents = ""    &= name "components"  &= name "c" &= typ "NAMES" &= explicit &= help "Components to include" &= groupname "Options"
    , optArches     = ""    &= name "arches"      &= name "a" &= typ "NAMES" &= explicit &= help "Architectures to include"
    , optCheckSums  = False &= name "check-sums"  &= name "s"                &= explicit &= help "Check package sums"
    , optCheckDups  = False &= name "check-dups"  &= name "d"                &= explicit &= help "Check package duplicates"
    , optCheckVers  = False &= name "check-vers"  &= name "v"                &= explicit &= help "Check package versions"
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

main :: IO ()
main = do
    args <- cmdArgs options

    let mirror     = argMirror args
        suites     = argSuites args
        components = words $ optComponents args
        arches     = words $ optArches     args

    when (null suites) $ error "Must specify suites"

    indexes <- getRepo mirror suites components arches

    when (not (optCheckSums args)
       && not (optCheckDups args)
       && not (optCheckVers args)) $
        forM_ indexes (putIndex mirror)

    when (optCheckSums args) $ checkSums indexes mirror
    when (optCheckDups args) $ checkDups indexes
    when (optCheckVers args) $ checkVers indexes

getRepo :: String -> [String] -> [String] -> [String] -> IO [Index]
getRepo mirror suites components arches =
    concat <$> forM (sort suites) (\s -> getSuite mirror s components arches)

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
        let release = either (const []) unControl $ parseControl location releaseData
            components = words . fieldValue' "Components"    =<< release
            arches     = words . fieldValue' "Architectures" =<< release
        return (components, arches ++ ["source"])
      where
        fieldValue' name = maybe "" B.unpack . fieldValue name

getIndex :: String -> (String, String, String) -> IO Index
getIndex m (s, c, a) = do
    let suiteBase = takeWhile (/= '/') s -- Handle eg stretch/updates
        location = m </> "dists" </> suiteBase </> c </> archIndex a
        onError err = do putErr "Parse error" err
                         return $ Control []
    result <- parseControl location <$> readZipped location
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
    showPackage p = printf "%s %s %s %s %s" s c a (pkgName p) (showVersion . pkgVersion $ p)

checkSums :: [Index] -> String -> IO ()
checkSums indexes mirror =
    forM_ indexes (checkIndex mirror)

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
    forM_ (filter (not . null) . lines $ pfs) $ \l ->
        case split ' ' l of
            [_, pmd, psz, pfn] ->
                checkPackageFile (s, c, a) pnm (m </> pdr </> pfn) (read psz) pmd
            _ -> error $ "Unreadable files line for source package " <> pkgName p <> ": " <> l

checkPackageFile :: (String, String, String) -> String -> FilePath -> Integer -> String -> IO ()
checkPackageFile (s, c, a) pnm fnm psz pmd = do
    exists <- doesFileExist fnm
    if not exists
        then putErr $ printf "File missing %s" fnm
        else do
            fsz <- toInteger . fileSize <$> getFileStatus fnm
            if fsz /= psz
                then putErr $ printf "Size mismatch: %d instead of %d" fsz psz
                else do
                    fmd <- md5 fnm
                    when (fmd /= pmd) $
                        putErr $ printf "Checksum mismatch: %s instead of %s" fmd pmd
  where
    putErr :: String -> IO ()
    putErr msg = hPutStrLn stderr $ printf "%s/%s/%s/%s/%s: %s" s c a pnm (takeFileName fnm) msg

checkDups :: [Index] -> IO ()
checkDups indexes =
    let pkgInst (s, c, a) p = ((pkgName p, pkgVersion p, a), (s, c, pkgHash p))
        ixInsts (Index s c a l) = map (pkgInst (s, c, a)) . unControl $ l
        allInsts = combineAssocs . concatMap ixInsts
        uniqueInsts = (map . second) (nubBy sameHash) . allInsts
        dupInsts = filter ((>1) . length . snd) . uniqueInsts
        sameHash = (==) `on` thd
        thd (_, _, x) = x
    in
        forM_ (dupInsts indexes) $ \((n, v, a), insts) -> do
            hPutStrLn stderr $ printf "%s %s %s" n (showVersion v) a
            forM_ insts $ \(s, c, h) ->
                hPutStrLn stderr $ printf "%12s %-12s %s" s c h

data PkgInstance = PkgInstance
    { instSuite     :: String
    , instComponent :: String
    , instArch      :: String
    , instPkg       :: Package }

instName :: PkgInstance -> String
instName    = pkgName    . instPkg
instVersion :: PkgInstance -> DebianVersion
instVersion = pkgVersion . instPkg

instance Show PkgInstance where
    show i = unwords $ map ($ i) [instSuite, instComponent, instArch, showVersion . instVersion]

checkVers :: [Index] -> IO ()
checkVers indexes =
    let ixInsts (Index s c a l) = map (PkgInstance s c a) . unControl $ l
        pkgInsts = groupSortBy instName . concatMap ixInsts
        suites = nub . sort . map ixSuite $ indexes
    in
        mapM_ (checkPackageVersions suites) (pkgInsts indexes)

type Suite = String

data VersionError
    = MissingSuites [Suite]
    | MultipleComponents [Suite]
    | InconsistentArches [Suite]
    | DecreasingVersions [(Suite, Suite)]

showVersionError :: VersionError -> String

showVersionError (MissingSuites suites) =
    printf "is missing from some suites: %s" (intercalate ", " suites)

showVersionError (MultipleComponents comps) =
    printf "is in multiple components: %s" (intercalate ", " comps)

showVersionError (InconsistentArches suites) =
    printf "has inconsistent versions within suites: %s"
        (intercalate ", " suites)

showVersionError (DecreasingVersions drops) =
    printf "has inconsistent versions across suites: %s"
        (intercalate ", " $ map showDrop drops)
  where showDrop (a, b) = a ++ " > " ++ b

checkPackageVersions :: [Suite] -> [PkgInstance] -> IO ()
checkPackageVersions suites insts = do
    let checks =
            [ checkMissingSuites suites
            , checkMultipleComponents
            , checkInconsistentArches
            , checkDecreasingVersions ]
        errors = concatMap ($ insts) checks
        name = maybe "unknown" (pkgName . instPkg) $ listToMaybe insts
        maintainers = nub . map (pkgMaintainer . instPkg) $ insts
    unless (null errors) $ do
        hPutStrLn stderr $ printf "%s: %s" name (intercalate ", " maintainers)
        forM_ errors $
            hPutStrLn stderr . printf "  * %s" . showVersionError
        forM_ insts $
            hPutStrLn stderr . ("  " ++) . show

checkMissingSuites :: [Suite] -> [PkgInstance] -> [VersionError]
checkMissingSuites suites insts =
    -- Present in all later suites after the first
    let inSuites = nub . sort . map instSuite $ insts
        missingFrom = case inSuites of
            (s : _) -> dropWhile (/= s) suites \\ inSuites
            _ -> []
    in [MissingSuites missingFrom | not $ null missingFrom]

checkMultipleComponents :: [PkgInstance] -> [VersionError]
checkMultipleComponents insts =
    -- Same component in all suites
    let comps = nub . map instComponent $ insts
    in [MultipleComponents comps | (_:_:_) <- comps]

checkInconsistentArches :: [PkgInstance] -> [VersionError]
checkInconsistentArches insts =
    -- Same version across arches in one suite
    let suiteInsts = groupSortBy instSuite insts
        problems = filter (not . allSameVersion . sort . map instVersion) suiteInsts
        allSameVersion (ver:vers) = all (ver `verIsPrefixOf`) vers
        allSameVersion _ = True
        verIsPrefixOf ver1 ver2 =
            let (e1, v1, r1) = evr ver1
                (e2, v2, r2) = evr ver2
            in  ver1 == ver2
                || e1 == e2
                   && (v1 `isPrefixOf` v2
                       || v1 == v2
                          && (fromMaybe "" r1 `isPrefixOf` fromMaybe "" r2))
    in [InconsistentArches $ concatMap (map instSuite . take 1) problems | not $ null problems]

checkDecreasingVersions :: [PkgInstance] -> [VersionError]
checkDecreasingVersions insts =
    -- Increasing version across suites
    let suiteInsts = groupSortBy instSuite insts
        suiteVers = map (minimumBy $ comparing instVersion) suiteInsts
        drops = disordersBy ((<=) `on` instVersion) suiteVers
    in [DecreasingVersions $ map (both instSuite) drops | not $ null drops]

both :: (a -> b) -> (a, a) -> (b, b)
both f = f *** f

isOrderedBy :: (a -> a -> Bool) -> [a] -> Bool
isOrderedBy rel xs = and $ zipWith rel xs (drop 1 xs)

disordersBy :: (a -> a -> Bool) -> [a] -> [(a, a)]
disordersBy rel xs = filter (not . uncurry rel) $ zip xs (drop 1 xs)

groupSortBy :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortBy field = map NE.toList . NE.groupAllWith field

combineAssocs :: Ord a => [(a, b)] -> [(a, [b])]
combineAssocs = map combine . NE.groupAllWith fst
  where combine g = (fst $ NE.head g, NE.toList $ NE.map snd g)

pkgCompare :: Package -> Package -> Ordering
pkgCompare p1 p2 =
    (compare `on` pkgName) p1 p2
        ||| (compare `on` pkgVersion) p1 p2
        ||| (compare `on` pkgArch) p1 p2
  where
    (|||) a b = if a /= EQ then a else b

pkgName :: Package -> String
pkgName = maybe "Unnamed" B.unpack . fieldValue "Package"

#if !MIN_VERSION_debian(3,89,0)
parseDebianVersion' = parseDebianVersion
#endif

pkgVersion :: Package -> DebianVersion
pkgVersion = parseDebianVersion' . maybe "Unversioned" B.unpack . fieldValue "Version"

pkgArch :: Package -> String
pkgArch = maybe "None" B.unpack . fieldValue "Architecture"

pkgMaintainer :: Package -> String
pkgMaintainer = maybe "None" B.unpack . fieldValue "Maintainer"

pkgHash :: Package -> String
pkgHash p = if pkgIsSrc p
    then dsc !! 1
    else pkgField "MD5sum" p
  where
    infos = map (split ' ') . filter (not . null) . lines . pkgField "Files" $ p
    dsc = fromJust . find ((== ".dsc") . takeExtension . (!! 3)) $ infos

pkgFile :: Package -> String
pkgFile p = if pkgIsSrc p
    then dsc !! 3
    else pkgField "Filename" p
  where
    infos = map (split ' ') . filter (not . null) . lines . pkgField "Files" $ p
    dsc = fromJust . find ((== ".dsc") . takeExtension . (!! 3)) $ infos

pkgIsSrc :: Package -> Bool
pkgIsSrc = isJust . fieldValue "Format"

pkgField :: String -> Package -> String
pkgField f p = case fieldValue f p of
    Nothing -> error $ "No field " ++ f ++ " in package " ++ pkgName p
    Just v -> B.unpack v

readZipped :: String -> IO B.ByteString
readZipped location = decompress <$> readLazyURI location
  where decompress = B.concat . LB.toChunks . GZip.decompress

readLazyURI :: String -> IO LB.ByteString
readLazyURI url = either err return =<< openLazyURIWithOpts [CurlFollowLocation True] url
  where err msg = error $ msg ++ " (" ++ url ++ ")"

readURI :: String -> IO B.ByteString
readURI url = either err return =<< openURIWithOpts [CurlFollowLocation True] url
  where err msg = error $ msg ++ " (" ++ url ++ ")"

putErr :: String -> ParseError -> IO ()
putErr msg e = hPutStrLn stderr $ msg ++ ": " ++ show e

md5 :: FilePath -> IO String
md5 fp = do
    h <- MD5.hashlazy <$> LB.readFile fp
    return $ showHexBytes h ""

showVersion :: DebianVersion -> String
showVersion = show . prettyDebianVersion

showHexBytes :: B.ByteString -> String -> String
showHexBytes bs s = foldr showHexByte s (B.unpack bs)
showHexByte :: Enum a => a -> String -> [Char]
showHexByte b = drop 1 . showHex (0x100 + fromEnum b)

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split c s  = h : split c (drop 1 t)
  where (h, t) = break (== c) s

final :: Integral n => n -> [a] -> [a]
final n xs = drop (length xs - fromIntegral n) xs
