{-# LANGUAGE DeriveGeneric #-}
module Main where
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Data.Maybe(fromJust)
import Data.Tree(flatten, unfoldTree)
import Diagrams.Backend.SVG.CmdLine(defaultMain)
import Diagrams.Prelude
import GHC.Generics

main       = defaultMain $ pad 1.1 $ renderTree buildTree
renderTree = mconcat . flatten . fmap drawBranch
buildTree  = unfoldTree (growTree fs) seed
seed       = (origin, unitY)
drawBranch (p, v) = position [(p, fromOffsets [v])]

growTree fs n = if stop n then branchTip n else branches fs n
branchTip   n = (n, [])
branches fs n = (n, zipWith ($) fs (repeat n))

fs = buildXfm symMap id rule

-- The rule has been transformed from "br[<B][>B]"
rule = ["b", "r", "<", ",", "b", "r", ">"]

symbols = (map toSymbolMap . filter isTerminal) grammar

isTerminal (Follow _)  = True
isTerminal (Scale _ _) = True
isTerminal (Turn  _ _) = True
isTerminal _           = False

toSymbolMap (Follow s)  = (s, followBranch)
toSymbolMap (Scale s r) = (s, scaleBranch r)
toSymbolMap (Turn  s t) = (s, rotateBranch t)
toSymbolMap t           = error $ "Bad terminal in toSymbolMap:" ++ show t

type SeedVal = ((Double, Double), (Double, Double))

type Grammar = [GrammarDef]
data GrammarDef = Var         String
                | Turn        String Double
                | Scale       String Double
                | Follow      String
                | Seed        String SeedVal
                | Rule        String String
    deriving (Show, Generic)

instance FromJSON GrammarDef
instance ToJSON GrammarDef

-- What I would like:
-- jsonInputWanted =
--     "[\
--     \{\"b\": {\"follow\":null}},\
--     \{\"r\": {\"scale\": 0.6}},\
--     \{\"<\": {\"turn\": 0.45}},\
--     \{\">\": {\"turn\": -0.45}},\
--     \{\"B\": {\"var\": null}},\
--     \{\"s\": {\"seed\": {\"p\": {\"x\":0, \"y\":0}, \"v\":{\"x\":0, \"y\":1}}}},\
--     \{\"R1\": {\"rule\": {\"B\": \"b r [ < B ] [ > B ]\"}}}\
--     \]"
    
-- What I have to use with generically derived instance
jsonInput =
    "[\
    \{\"contents\":\"b\",\"tag\":\"Follow\"},\
    \{\"contents\":\"B\",\"tag\":\"Var\"},\
    \{\"contents\":[\"r\",0.6],\"tag\":\"Scale\"},\
    \{\"contents\":[\"\\u003c\",0.14285714285714],\"tag\":\"Turn\"},\
    \{\"contents\":[\"\\u003e\",-0.14285714285714],\"tag\":\"Turn\"},\
    \{\"contents\":[\"s\",[[0,0],[0,1]]],\"tag\":\"Seed\"},\
    \{\"contents\":[\"R1\",\"br<,br>\"],\"tag\":\"Rule\"}\
    \]"

grammar = case decode (BS.pack jsonInput) of
    (Just g) -> g
    Nothing  -> error $ "Bad grammar in JSON: " ++ jsonInput

-- Assumes string has been converted to a comma-separated list of
-- terminal symbols corresponding to transformations.
buildXfm _ f []       = f : []
buildXfm m f (",":xs) = f : buildXfm m id xs
buildXfm m f (x:xs)   = buildXfm m (g . f) xs
    where g = fromJust (M.lookup x m)

symMap          = initMap symbols
initMap         = foldr updateMap M.empty 
updateMap (k,f) = M.insert k f

stop           (_, v) = magnitude v < 0.05
followBranch   (p, v) = (p .+^ v, v)
scaleBranch  s (p, v) = (p, v ^* s)
rotateBranch t (p, v) = (p, rotateBy t v)
