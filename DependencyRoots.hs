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
import Data.Tuple.Utils
import Data.Function
import Control.Monad
import System.IO

import qualified Data.ByteString.Char8 as B

type Package = Paragraph
type FieldValue = B.ByteString
type PackageName = FieldValue

main = processFilePathsWith $ parseControlFromFile
            >=> either (putErr "Parse error") (putForest)

putErr :: String -> ParseError -> IO ()
putErr msg e = hPutStrLn stderr $ msg ++ ": " ++ show e

putRoots :: Control -> IO ()
putRoots = mapM_ putStrLn . sort . packageRoots

putForest :: Control -> IO ()
putForest = putStr . drawForest . sortBy cmpRoot . packageForest
    where cmpRoot = compare `on` rootLabel

packageGraph :: Control -> (Gr String (), NodeMap String)
packageGraph c =
    let pkgs = filter pkgIsInstalled . unControl $ c
        nodes = map pkgName pkgs
        edges = concatMap mkEdges pkgs
        mkEdges p = map (mkEdge p) . filter installed . pkgDeps $ p
        mkEdge p d = (pkgName p, d, ())
        installed name = name `elem` nodes
    in mkMapGraph nodes edges

packageForest :: Control -> Forest String
packageForest c = map (fmap $ fromMaybe "" . lab g) $ dff (topsort g) g
    where g = fst . packageGraph $ c

packageRoots :: Control -> [String]
packageRoots = map rootLabel . packageForest

pkgName :: Package -> String
pkgName = B.unpack . fromMaybe (B.pack "Unnamed") . fieldValue "Package"

pkgIsInstalled :: Package -> Bool
pkgIsInstalled = maybe False installed . fieldValue "Status"
    where parseStatus = B.split ' ' . stripWS
          installed v = parseStatus v !! 2 == B.pack "installed"

pkgDeps :: Package -> [String]
pkgDeps p =
    let field = B.unpack . fromMaybe B.empty . flip fieldValue p
        rels = fromRight . parseRelations . field
        names = map (relName . head) . rels
        relName (Rel name _ _) = name
    in names "Depends" ++ names "Recommends"
