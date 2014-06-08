{-# LANGUAGE NoMonomorphismRestriction, DeriveGeneric, OverloadedStrings #-}
module Main where

import Data.Tree(Tree(..), flatten, unfoldTree)
import Diagrams.Backend.SVG.CmdLine(defaultMain)
import Diagrams.Prelude

-- Imports for parsing grammar from JSON
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Data.Maybe(fromJust)
import GHC.Generics

-- Imports for parsing rules
import Control.Applicative
import Data.Attoparsec.Char8
--import Data.Attoparsec.Text
import qualified Data.HashMap.Strict as HM
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V


main = defaultMain $ pad 1.1 $ renderTree $ buildTree

renderTree        = mconcat . flatten . fmap drawBranch
drawBranch (p, v) = position [(p, fromOffsets [v])]

buildTree     = unfoldTree (growTree fs) seed
growTree fs n = if stop n then branchTip n else branches fs n
branchTip   n = (n, [])
branches fs n = (n, zipWith ($) fs (repeat n))

-- Seed needs to be part of grammar.
seed = (origin, unitY)

-- Build branching transformations to apply to each node
fs = buildXfm symToFunc rule

-- The rule has been transformed from "b r [ < B ] [ > B ]"
--rule = [["b", "r", "<"], ["b", "r", ">"]]
--rule = (elimVars stringToGrammarDef . distribute . parseRule) userRule
rule = getRule userRule

getRule = (elimVars stringToGrammarDef . distribute . parseRule)

userRule = "b r [ < B ] [ > B ]"

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
jsonInputGeneric =
    "[\
    \{\"contents\":\"b\",\"tag\":\"Follow\"},\
    \{\"contents\":\"B\",\"tag\":\"Var\"},\
    \{\"contents\":[\"r\",0.6],\"tag\":\"Scale\"},\
    \{\"contents\":[\"\\u003c\",0.14285714285714],\"tag\":\"Turn\"},\
    \{\"contents\":[\"\\u003e\",-0.14285714285714],\"tag\":\"Turn\"},\
    \{\"contents\":[\"s\",[[0,0],[0,1]]],\"tag\":\"Seed\"},\
    \{\"contents\":[\"R1\",\"br<,br>\"],\"tag\":\"Rule\"}\
    \]"

--
jsonInputBetter =
    "[\
    \{\"follow\":\"b\"},\
    \{\"var\":\"B\"},\
    \{\"scale\":{\"r\":0.6}},\
    \{\"turn\":{\"<\":0.14285714285714}},\
    \{\"turn\":{\">\":-0.14285714285714}},\
    \{\"seed\":{\"s\":[[0,0],[0,1]]}},\
    \{\"rule\":{\"R1\":\"br<,br>\"}}\
    \]"
--
jsonInputEvenBetter =
    "[\
    \{\"b\":\"follow\"},\
    \{\"B\":\"var\"},\
    \{\"r\":{\"scale\":0.6}},\
    \{\"<\":{\"turn\":0.14285714285714}},\
    \{\">\":{\"turn\":-0.14285714285714}},\
    \{\"s\":{\"seed\":{\"p\":{\"x\":0,\"y\":0},\"v\":{\"x\":0,\"y\":1}}}},\
    \{\"R1\":{\"rule\": {\"B\": \"b r [ < B ] [ > B ]\"}}}\
    \]"

type SeedVal = ((Double, Double), (Double, Double))
type RuleVal = (String, String)
type Grammar = [GrammarDef]
data GrammarDef = Var         String
                | Turn        String Double
                | Scale       String Double
                | Follow      String
                | Seed        String SeedVal
                | Rule        String RuleVal
    deriving (Show, Generic)

instance FromJSON GrammarDef
instance ToJSON GrammarDef

-- grammar' :: Grammar
-- grammar' =
--   [
--       Follow "b"
--     , Var    "B"
--     , Scale  "r"  0.6
--     , Turn   "<"  0.45
--     , Turn   ">"  (-0.45)
--     , Seed   "s"  ((0, 0), (0, 1))
--     , Rule   "R1" ("B", "b r [ B < ] [ B > ]")
--   ]

-- Should produce same value as grammar'.
parseGrammar j = case decode (BS.pack j) of
    (Just g) -> g
    Nothing  -> error $ "Bad grammar in JSON: " ++ j


grammar = parseGrammar jsonInputGeneric

grammar' :: Value
grammar' = case parseOnly json jsonInputEvenBetter of
    Left  e -> error $ "Bad grammar in JSON: " ++ jsonInputEvenBetter
    Right g -> g

toGrammarMap'  (Array vs) = V.map toGrammarMap'' vs
toGrammarMap'' (Object o) = result
    where
        getObject s k o = case HM.lookup k o of
                    Just (Object x)  -> x
                    Nothing -> error $ "seedToGrammarMap: " ++ s ++ " missing"
        
        ks = HM.keys o
        s = if length ks == 1
            then T.unpack (head ks)
            else error $ "Only one key allowed per top-level grammar element but " ++ show (length ks) ++ " found"
        rhs = HM.lookup (T.pack s) o
        
        result = case rhs of
              Just (String "var")    -> (s, Var s)
              Just (String "follow") -> (s, Follow s)
              Just (Object o') ->
                let ks' = HM.keys o'
                    k = if length ks' == 1
                        then head ks'
                        else error $ "Only one key allowed per grammar element but " ++ show (length ks') ++ " found"
                    rhs' = HM.lookup k o'
                in case k of
                      "turn" -> case rhs' of
                          Just (Number n') -> (s, Turn s (toRealFloat n'))
                          otherwise -> error $ "Bad value for turn: " ++ BS.unpack (encode rhs')
                      "scale" -> case rhs' of
                          Just (Number n') -> (s, Scale s (toRealFloat n'))
                          otherwise -> error $ "Bad value for scale: " ++ BS.unpack (encode rhs')
                      "rule" -> case rhs' of
                          Just rhs''@(Object o'') -> ruleToGrammarMap s rhs''
                          otherwise -> error $ "Bad value for rule: " ++ BS.unpack (encode rhs')
                      "seed" -> case rhs' of
                          Just rhs''@(Object o'') -> seedToGrammarMap s rhs''
                          otherwise -> error $ "Bad value for seed: " ++ BS.unpack (encode rhs')
              otherwise -> error $ "Bad value for grammar element: " ++ BS.unpack (encode rhs)

toGrammarMap'' v = error $ "Bad grammar element " ++ BS.unpack (encode v)

ruleToGrammarMap s (Object o) = (s, Rule s (T.unpack lhs, T.unpack rhs))
    where
        ks = HM.keys o
        lhs = if length ks == 1
            then head ks
            else error $ "Only one rule supported but " ++ show (length ks) ++ " found"
        rhs = case HM.lookup lhs o of
                Just (String rhs') -> rhs'
                otherwise -> error $ "ruleToGrammarMap: bad key: " ++ T.unpack lhs

seedToGrammarMap s (Object o) = (s, Seed s ((pxval, pyval), (vxval, vyval)))
    where
        pval  = getObject "pval" "p" o
        vval  = getObject "vval" "v" o
        pxval = toRealFloat $ getNumber "pxval" "x" pval
        pyval = toRealFloat $ getNumber "pyval" "y" pval
        vxval = toRealFloat $ getNumber "vxval" "x" vval
        vyval = toRealFloat $ getNumber "vyval" "y" vval
        
        getNumber s k o = case HM.lookup k o of
                    Just (Number x)  -> x
                    Nothing -> error $ "seedToGrammarMap: " ++ s ++ " missing"
        getObject s k o = case HM.lookup k o of
                    Just (Object x)  -> x
                    Nothing -> error $ "seedToGrammarMap: " ++ s ++ " missing"

stringToGrammarDef' = initMap $ (V.toList . toGrammarMap') grammar'

toGrammarMap g@(Var    s)   = (s, g)
toGrammarMap g@(Follow s)   = (s, g)
toGrammarMap g@(Scale  s _) = (s, g)
toGrammarMap g@(Turn   s _) = (s, g)
toGrammarMap g@(Seed   s _) = (s, g)
toGrammarMap g@(Rule   s _) = (s, g)

stringToGrammarDef = initMap (map toGrammarMap grammar)

-- symbols' = [
--       ("b", followBranch)
--     , ("r", scaleBranch 0.6)
--     , ("<", rotateBranch (1/7))
--     , (">", rotateBranch (-1/7))
--     ]

symbols = (map toSymbolMap . filter isTerminal) grammar

isTerminal (Follow _)  = True
isTerminal (Scale _ _) = True
isTerminal (Turn  _ _) = True
isTerminal _           = False

toSymbolMap (Follow s)  = (s, followBranch)
toSymbolMap (Scale s r) = (s, scaleBranch r)
toSymbolMap (Turn  s t) = (s, rotateBranch t)
toSymbolMap t           = error $ "Bad terminal in toSymbolMap:" ++ show t

-- The termination condition, which should be overridable on the command line
stop           (_, v) = magnitude v < 0.05

-- Elementary transformations
followBranch   (p, v) = (p .+^ v, v)
scaleBranch  s (p, v) = (p, v ^* s)
rotateBranch t (p, v) = (p, rotateBy t v)

-- Assumes string has been denormalized to a list of lists of
-- terminal symbols corresponding to transformations.
buildXfm :: Map String ((P2, R2) -> (P2, R2))
         -> [[String]]
         -> [(P2, R2) -> (P2, R2)]
buildXfm _ []        = []
buildXfm m (xs:xss)  = (buildXfm' m id xs) : buildXfm m xss
buildXfm' _ f []     = f
buildXfm' m f (x:xs) = buildXfm' m (g . f) xs
    where g = fromJust (M.lookup x m)

symToFunc = initMap symbols
initMap = foldr updateMap M.empty 
updateMap (k,f) m = M.insert k f m

--parseRule :: Text -> Tree [String]
parseRule r = case parseOnly tree r of
    Left e  -> error $ "Could not parse rule: " ++ show r ++ " (" ++ e ++ ")"
    Right t -> t

tree :: Parser (Tree [String])
tree =  (do ts <- many1 subTree
            return $ Node [] ts)
    <|> (do xs <- flatList
            ts <- many1 subTree
            return $ Node xs ts)
    <|> (do xs <- flatList
            return $ Node xs [])

-- One level of recursion only
subTree :: Parser (Tree [String])
subTree = do skipSpace
             char '['
             xs <- flatList
             skipSpace
             char ']'
             return $ Node xs []

flatList :: Parser [String]
flatList = many1 idToken

idToken :: Parser String
idToken = skipSpace >> many1 (satisfy (notInClass "[] \t\n"))

-- One level of recursion.
distribute (Node p []) = [p]
distribute (Node p ts) = map (p ++) (map rootLabel ts)

elimVars m = map (filter (not . isVar m))
isVar m s = case M.lookup s m of
    Just (Var _) -> True
    otherwise    -> False
