{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Debian.Control.ByteString
import Debian.Relation

import Data.Graph.Inductive
import Data.Tree
import Data.Set (fromList, member)
import Data.List (find, intercalate, sortOn)
import Data.Maybe
import Data.Either
import System.IO

import System.Console.CmdArgs.Implicit

import qualified Data.ByteString.Char8 as B

#if !MIN_VERSION_base(4,10,0)
fromRight :: b -> Either a b -> b
fromRight d = either (const d) id
#endif

type Package = Paragraph
type FieldValue = B.ByteString
type PackageName = FieldValue

data Style = Roots | Forest
    deriving (Show, Data, Typeable)

data Options = Options
    { statusFile :: String
    , style      :: Style }
    deriving (Show, Data, Typeable)

options :: Options
options = Options
    { statusFile = def &= typ "STATUSFILE" &= argPos 0 &= opt "/var/lib/dpkg/status"
    , style = enum [Roots &= help "Show dependency roots (default)", Forest &= help "Show dependency forest"]
            &= groupname "Options" }
        &= program "DependencyRoots"
        &= summary "DependencyRoots v0.5"
        &= details ["STATUSFILE defaults to /var/lib/dpkg/status"]

main :: IO ()
main = do
    args <- cmdArgs options
    parseControlFromFile (statusFile args)
        >>= either (putErr "Parse error") (putDeps (style args) . packageDeps)
  where putDeps style = case style of
            Roots  -> putRoots graphRoots  showAlts
            Forest -> putRoots graphForest showTree
        showTree = drawTree
        showAlts = intercalate "|" . flatten

putErr :: Show e => String -> e -> IO ()
putErr msg e = hPutStrLn stderr $ msg ++ ": " ++ show e

putRoots :: (Gr String () -> Forest String) -> (Tree String -> String) -> [[String]] -> IO ()
putRoots fRoots fShow = mapM_ (putStrLn . fShow) . sortForest . fRoots . makeGraph
  where sortForest = sortOn rootLabel

graphRoots :: Gr a b -> Forest a
graphRoots g = map labelAlts alternates
  where forest = dff (topsort g) g
        alternates = map (ancestors . rootLabel) forest
        ancestors n = head $ rdff [n] g
        labelAlts = fmap (fromJust . lab g)

graphForest :: Gr a b -> Forest a
graphForest g = map labelTree forest
  where forest = dff (topsort g) g
        labelTree = fmap (fromJust . lab g)

makeGraph :: [[String]] -> Gr String ()
makeGraph deps = fst $ mkMapGraph nodes edges
  where nodes = map head deps
        edges = concatMap mkEdges deps
        mkEdges (n : sucs) = map (n,, ()) sucs
        mkEdges _ = error "Empty deps"

packageDeps :: Control -> [[String]]
packageDeps c = map mkDeps pkgs
  where pkgs = filter pkgIsInstalled . unControl $ c
        names = fromList . map extName $ pkgs
        mkDeps p = extName p : filter installed (pkgDeps p)
        installed name = name `member` names
        extName p = if a /= baseArch && a /= "all" then n ++ ':' : a else n
          where n = pkgName p
                a = pkgArch p
        baseArch = maybe "" pkgArch $ find (\p -> pkgName p == "base-files") pkgs

pkgName :: Package -> String
pkgName = maybe "Unnamed" B.unpack . fieldValue "Package"

pkgArch :: Package -> String
pkgArch = maybe "" B.unpack . fieldValue "Architecture"

pkgIsInstalled :: Package -> Bool
pkgIsInstalled = maybe False isInstalled . fieldValue "Status"
  where isInstalled v = parseStatus v !! 2 == B.pack "installed"
        parseStatus = B.split ' ' . stripWS

#if !MIN_VERSION_debian(3,64,0)
unBinPkgName = id
#elif !MIN_VERSION_debian(3,69,0)
unBinPkgName_ = unPkgName . unBinPkgName
#define unBinPkgName unBinPkgName_
#endif

pkgDeps :: Package -> [String]
pkgDeps p = names "Depends" ++ names "Recommends"
  where field = B.unpack . fromMaybe B.empty . flip fieldValue p
        rels = fromRight [] . parseRelations . field
        names = map (relName . head) . rels
        relName (Rel name _ _) = unBinPkgName name
