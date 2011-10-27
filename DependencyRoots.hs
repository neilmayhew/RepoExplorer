module Main where

import Utilities

import Debian.Control.ByteString
import Debian.Relation
import Text.ParserCombinators.Parsec.Error

import Data.Graph.Inductive
import Data.Tree
import Data.List
import Data.Maybe
import Data.Either
import Data.Either.Utils
import Data.Ord
import Control.Monad
import System.IO

import qualified Data.ByteString.Char8 as B

type Package = Paragraph
type FieldValue = B.ByteString
type PackageName = FieldValue

main = processFilePathsWith $ parseControlFromFile
            >=> either (putErr "Parse error") (putForest drawTree . packageForest)

putErr :: String -> ParseError -> IO ()
putErr msg e = hPutStrLn stderr $ msg ++ ": " ++ show e

putForest :: (Ord a, Show a) => (Tree a -> String) -> Forest a -> IO ()
putForest f = mapM_ putStrLn . map f . sortBy (comparing rootLabel)

graphForest :: Gr a b -> Forest a
graphForest g = map labelTree forest
  where forest = dff (topsort g) g
        labelTree = fmap (fromJust . lab g)

packageForest :: Control -> Forest String
packageForest = graphForest . fst . packageGraph

packageGraph :: Control -> (Gr String (), NodeMap String)
packageGraph c = mkMapGraph nodes edges
  where pkgs = filter pkgIsInstalled . unControl $ c
        nodes = map pkgName pkgs
        edges = concatMap mkEdges pkgs
        mkEdges p = map (mkEdge p) . filter installed . pkgDeps $ p
        mkEdge p d = (pkgName p, d, ())
        installed name = name `elem` nodes

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
        relName (Rel name _ _) = name
