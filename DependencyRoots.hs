{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Debian.Control.ByteString
import Debian.Relation

import Data.Graph.Inductive
import Data.Tree
import Data.Set (fromList, member)
import Data.List
import Data.Maybe
import Data.Either
import Data.Either.Utils
import Data.Ord
import Control.Monad
import System.IO

import System.Console.CmdArgs.Implicit

import qualified Data.ByteString.Char8 as B

type Package = Paragraph
type FieldValue = B.ByteString
type PackageName = FieldValue

data Style = Roots | Forest
    deriving (Show, Data, Typeable)

data Options = Options
    { statusFile :: String
    , style      :: Style }
    deriving (Show, Data, Typeable)

options = Options
    { statusFile = def &= typ "STATUSFILE" &= argPos 0 &= opt "/var/lib/dpkg/status"
    , style = enum [Roots &= help "Show dependency roots (default)", Forest &= help "Show dependency forest"]
            &= groupname "Options" }
        &= program "DependencyRoots"
        &= summary "DependencyRoots v0.5"
        &= details ["STATUSFILE defaults to /var/lib/dpkg/status"]

main = do
    args <- cmdArgs options
    (parseControlFromFile $ statusFile args)
        >>= either (putErr "Parse error") (putDeps (style args) . packageDeps)
  where putDeps style = case style of
            Roots  -> putRoots graphRoots  showAlts
            Forest -> putRoots graphForest showTree
        showTree = drawTree
        showAlts = intercalate "|" . flatten

putErr :: Show e => String -> e -> IO ()
putErr msg e = hPutStrLn stderr $ msg ++ ": " ++ show e

putRoots :: (Gr String () -> Forest String) -> (Tree String -> String) -> [[String]] -> IO ()
putRoots fRoots fShow = mapM_ putStrLn . map fShow . sortForest . fRoots . makeGraph
  where sortForest = sortBy (comparing rootLabel)

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
        mkEdges (n : sucs) = map (\s -> (n, s, ())) sucs

packageDeps :: Control -> [[String]]
packageDeps c = map mkDeps pkgs
  where pkgs = filter pkgIsInstalled . unControl $ c
        names = fromList . map pkgName $ pkgs
        mkDeps p = pkgName p : filter installed (pkgDeps p)
        installed name = name `member` names

pkgName :: Package -> String
pkgName = maybe "Unnamed" B.unpack . fieldValue "Package"

pkgIsInstalled :: Package -> Bool
pkgIsInstalled = maybe False isInstalled . fieldValue "Status"
  where isInstalled v = parseStatus v !! 2 == B.pack "installed"
        parseStatus = B.split ' ' . stripWS

pkgDeps :: Package -> [String]
pkgDeps p = names "Depends" ++ names "Recommends"
  where field = B.unpack . fromMaybe B.empty . flip fieldValue p
        rels = fromRight . parseRelations . field
        names = map (relName . head) . rels
        relName (Rel name _ _) = unPkgName . unBinPkgName $ name
