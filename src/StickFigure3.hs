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
    tcInitialWidth :: Double,
    tcBranchScale  :: Double,
    tcBranchAngle  :: Double
} deriving (Show)

--tc :: TreeConfig
tc = TC {
    tcScale        = s,
    tcCutOff       = 0.05 * s,
    tcInitialWidth = 0.01 * s,
    tcBranchScale  = 0.6,
    tcBranchAngle  = 1/7
}
    where s = 10000

--main :: IO ()
main = defaultMain $ pad 1.1 $ renderTree $ buildTree

--renderTree :: Tree TreeNode -> Dgm
renderTree = mconcat . flatten . fmap drawBranch

--buildTree :: Tree TreeNode
buildTree = unfoldTree branches seed

--seed :: TreeNode
seed = (origin, unitY ^* tcScale tc, tcInitialWidth tc)

--drawBranch :: TreeNode -> Dgm
drawBranch (p, v, w) = position [(p, fromOffsets [v])] # lw w

--branches :: TreeNode -> (TreeNode, [TreeNode])
branches n@(_, v, _)
    | magnitude v < tcCutOff tc  =  (n, [])
    | otherwise                  =  (n, branchTips n)

--branchTips :: TreeNode -> [TreeNode]
branchTips (p, v, w) = [(p', br a, w), (p', br (-a), w)]
    where p'   = p .+^ v
          br a = v # scale (tcBranchScale tc) # rotateBy a
          a    = tcBranchAngle tc
