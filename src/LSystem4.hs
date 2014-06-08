{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main where
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BS'
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Data.Maybe(fromJust)
import Data.Tree(Tree(..), flatten, unfoldTree)
import Diagrams.Backend.SVG.CmdLine(defaultMain)
import Diagrams.Prelude
import GHC.Generics
import Control.Applicative
import Data.Attoparsec.Char8

main       = defaultMain $ pad 1.1 $ renderTree buildTree
renderTree = mconcat . flatten . fmap drawBranch
buildTree  = unfoldTree (growTree fs) seed
seed       = case M.lookup "s" seedMap of
    Just s    -> s
    otherwise -> error $ "Coudn't find seed s"
drawBranch (p, v) = position [(p, fromOffsets [v])]

growTree fs n = if stop n then branchTip n else branches fs n
branchTip   n = (n, [])
branches fs n = (n, zipWith ($) fs (repeat n))

fs = buildXfm symMap rule

rule     = getRule $ BS'.pack userRule
getRule  = elimVars stringToGrammarDef . distribute . parseRule
userRule = case M.lookup "B" ruleMap of
    Just r    -> r
    otherwise -> error $ "Couldn't find production for variable B"

symbols = (map toSymbolMap . filter isTerminal) grammar
rules   = (map toRuleMap   . filter isRule)     grammar
seeds   = (map toSeedMap   . filter isSeed)     grammar

isTerminal (Follow _)  = True
isTerminal (Scale _ _) = True
isTerminal (Turn  _ _) = True
isTerminal _           = False

toSymbolMap (Follow s)  = (s, followBranch)
toSymbolMap (Scale s r) = (s, scaleBranch r)
toSymbolMap (Turn  s t) = (s, rotateBranch t)
toSymbolMap t           = error $ "Bad terminal in toSymbolMap:" ++ show t

isRule (Rule _ _)  = True
isRule _           = False

toRuleMap (Rule _ (v, r))  = (v, r)
toRuleMap r                = error $ "Bad rule in toRuleMap:" ++ show r

isSeed (Seed _ _)  = True
isSeed _           = False

toSeedMap (Seed s (p, v))  = (s, (p2 p, r2 v))
toSeedMap s                = error $ "Bad seed in toSeedMap:" ++ show s

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
    \{\"contents\":[\"<\",0.14285714285714],\"tag\":\"Turn\"},\
    \{\"contents\":[\">\",-0.14285714285714],\"tag\":\"Turn\"},\
    \{\"contents\":[\"s\",[[0,0],[0,1]]],\"tag\":\"Seed\"},\
    \{\"contents\":[\"R1\",[\"B\" ,\"b r [ < B ] [ > B ]\"]],\"tag\":\"Rule\"}\
    \]"

-- Demonstration of flexibility--one side of the Koch snowflake.
jsonKoch =
    "[\
    \{\"contents\":\"b\",\"tag\":\"Follow\"},\
    \{\"contents\":\"B\",\"tag\":\"Var\"},\
    \{\"contents\":[\"r\",0.33333333333333],\"tag\":\"Scale\"},\
    \{\"contents\":[\"<\",0.16666666666667],\"tag\":\"Turn\"},\
    \{\"contents\":[\">\",-0.16666666666667],\"tag\":\"Turn\"},\
    \{\"contents\":[\"s\",[[0,0],[1,0]]],\"tag\":\"Seed\"},\
    \{\"contents\":[\"R1\",[\"B\" ,\"[ r B ] [ r b < B ] [ r b < b > > B ] [ r b < b > > b < B ]\"]],\"tag\":\"Rule\"}\
    \]"

parseGrammar j = case decode (BS.pack j) of
    (Just g) -> g
    Nothing  -> error $ "Bad grammar in JSON: " ++ j

grammar = parseGrammar jsonInput
-- grammar = parseGrammar jsonKoch

toGrammarMap g@(Var    s)   = (s, g)
toGrammarMap g@(Follow s)   = (s, g)
toGrammarMap g@(Scale  s _) = (s, g)
toGrammarMap g@(Turn   s _) = (s, g)
toGrammarMap g@(Seed   s _) = (s, g)
toGrammarMap g@(Rule   s _) = (s, g)

stringToGrammarDef = initMap (map toGrammarMap grammar)

-- Assumes string has been converted to a list of lists of
-- terminal symbols corresponding to transformations.
buildXfm :: Map String ((P2, R2) -> (P2, R2))
         -> [[String]]
         -> [(P2, R2) -> (P2, R2)]
buildXfm _ []        = []
buildXfm m (xs:xss)  = (buildXfm' m id xs) : buildXfm m xss
buildXfm' _ f []     = f
buildXfm' m f (x:xs) = buildXfm' m (g . f) xs
    where g = fromJust (M.lookup x m)

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
isVar m s  = case M.lookup s m of
    Just (Var _) -> True
    otherwise    -> False

symMap          = initMap symbols
ruleMap         = initMap rules
seedMap         = initMap seeds
initMap         = foldr updateMap M.empty 
updateMap (k,f) = M.insert k f

stop           (_, v) = magnitude v < 0.05
followBranch   (p, v) = (p .+^ v, v)
scaleBranch  s (p, v) = (p, v ^* s)
rotateBranch t (p, v) = (p, rotateBy t v)
