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
            >=> either (putErr "Parse error") (putTree)

putErr :: String -> ParseError -> IO ()
putErr msg e = hPutStrLn stderr $ msg ++ ": " ++ show e

putRoots :: Control -> IO ()
putRoots = mapM_ putStrLn . sort . map rootLabel . packageTree

putTree :: Control -> IO ()
putTree = putStr . drawForest . sortBy cmpRoot . packageTree
    where cmpRoot = compare `on` rootLabel

packageGraph :: Control -> (Gr String (), NodeMap String)
packageGraph c =
    let ps = filter pkgIsInstalled . unControl $ c
        ds = map pkgDeps ps
        nodes = map pkgName ps `union` concat ds
        edges = concatMap mkEdges ps
        mkEdges p = map (\d -> (pkgName p, d, ())) (pkgDeps p)
    in mkMapGraph nodes edges

packageTree :: Control -> Forest String
packageTree c = map (fmap $ fromMaybe "" . lab g) $ dff' g
    where g = fst . packageGraph $ c

pkgName :: Package -> String
pkgName = B.unpack . fromMaybe (B.pack "Unnamed") . fieldValue "Package"

pkgIsInstalled :: Package -> Bool
pkgIsInstalled = maybe False installed . fieldValue "Status"
    where parseStatus = B.split ' ' . stripWS
          installed v = parseStatus v !! 2 == B.pack "installed"

pkgDeps :: Package -> [String]
pkgDeps p =
    let depsField = B.unpack $ fromMaybe B.empty $ fieldValue "Depends" p
        depsRels  = fromRight $ parseRelations depsField
        depsNames = map (relName . head) $ filter nonAlt depsRels
        recsField = B.unpack $ fromMaybe B.empty $ fieldValue "Recommends" p
        recsRels  = fromRight $ parseRelations recsField
        recsNames = map (relName . head) $ filter nonAlt recsRels
        relName (Rel name _ _) = name
        nonAlt r = length r == 1
    in depsNames ++ recsNames
