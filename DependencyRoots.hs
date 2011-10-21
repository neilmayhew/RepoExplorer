module Main where

import Utilities

import Debian.Control.ByteString
import Debian.Relation
import Text.ParserCombinators.Parsec.Error

import Data.Graph
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
putRoots = mapM_ (putStrLn . pkgName) . sortBy cmpName . map rootLabel . packageTree
    where cmpName = compare `on` fieldValue "Package"

putTree :: Control -> IO ()
putTree = putStr . drawForest . sortBy cmpRoot . map (fmap pkgName) . packageTree
    where cmpRoot = compare `on` rootLabel

packageTree :: Control -> Forest Package
packageTree c =
    let ps = filter pkgIsInstalled . unControl $ c
        (g, vmap, kmap) = graphFromEdges $ map mkNode ps
        mkNode p = (p, pkgName p, pkgDeps p)
        vertexData = fst3 . vmap
    in  map (fmap vertexData) (dff g)

pkgName :: Package -> String
pkgName = maybe "Unnamed" B.unpack . fieldValue "Package"

pkgIsInstalled :: Package -> Bool
pkgIsInstalled = maybe False installed . fieldValue "Status"
    where parseStatus = B.split ' ' . stripWS
          installed v = parseStatus v !! 2 == B.pack "installed"

pkgDeps :: Package -> [PkgName]
pkgDeps p =
    let depsField = maybe "" B.unpack $ fieldValue "Depends" p
        depsRels  = fromRight $ parseRelations depsField
        depsNames = map (relName . head) $ filter nonAlt depsRels
        recsField = maybe "" B.unpack $ fieldValue "Recommends" p
        recsRels  = fromRight $ parseRelations recsField
        recsNames = map (relName . head) $ filter nonAlt recsRels
        relName (Rel name _ _) = name
        nonAlt r = length r == 1
    in depsNames ++ recsNames
