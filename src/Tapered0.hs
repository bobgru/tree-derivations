module Main where
import Data.Tree(Tree, flatten, unfoldTree)
import Diagrams.Backend.SVG.CmdLine(defaultMain)
--import Diagrams.Backend.SVG(SVG)
import Diagrams.Prelude

--type TreeNode = (P2, R2, Double)
--type Dgm = Diagram SVG R2

data TreeConfig = TC {
    tcScale        :: Double,
    tcCutOff       :: Double,
    tcMinWidth     :: Double,
    tcInitialWidth :: Double,
    tcWidthTaper   :: Double,
    tcBranchScale  :: Double,
    tcBranchAngle  :: Double
} deriving (Show)

--tc :: TreeConfig
tc = TC {
    tcScale        = s,
    tcCutOff       = 0.05 * s,
    tcMinWidth     = 0.01 * s,
    tcInitialWidth = 0.1  * s,
    tcWidthTaper   = 0.7,
    tcBranchScale  = 0.6,
    tcBranchAngle  = 1/7
}
    where s = 10000

--main :: IO ()
main = defaultMain $ pad 1.1 $ renderTree buildTree

--renderTree :: Tree TreeNode -> Dgm
renderTree = mconcat . flatten . fmap drawBranch

--buildTree :: Tree TreeNode
buildTree = unfoldTree branches seed

--seed :: TreeNode
seed = (origin, unitY ^* tcScale tc, tcInitialWidth tc)

--drawBranch :: TreeNode -> Dgm
drawBranch n@(p, v, w) = place d p
    where d | w <= tcMinWidth tc  =  lineSegment v w
            | otherwise           =  trapezoid n

--lineSegment :: R2 -> Double -> Dgm
lineSegment v w     = fromOffsets [v] # lw w

--trapezoid :: TreeNode -> Dgm
trapezoid (p, v, w) = (closeLine . lineFromVertices) [ p, a, b, c, d ]
                    # strokeLoop # fc black # lw 0.01
    where p' = p .+^ v
          w' = taperWidth w
          n  = v # rotateBy (1/4) # normalized
          w2 = w  / 2 ; w2' = w' / 2
          a  = p  .-^ (w2  *^ n) ; b = p' .-^ (w2' *^ n)
          c  = p' .+^ (w2' *^ n) ; d = p  .+^ (w2  *^ n)

--taperWidth :: Double -> Double
taperWidth w = max (w * tcWidthTaper tc) (tcMinWidth tc)

--branches :: TreeNode -> (TreeNode, [TreeNode])
branches n@(_, v, _)
    | magnitude v < tcCutOff tc  =  (n, [])
    | otherwise                  =  (n, branchTips n)

--branchTips :: TreeNode -> [TreeNode]
branchTips (p, v, w) = [(q, br a, w'), (q, br (-a), w')]
    where q    = p .+^ v
          br a = v # scale (tcBranchScale tc) # rotateBy a
          a    = tcBranchAngle tc
          w'   = taperWidth w
