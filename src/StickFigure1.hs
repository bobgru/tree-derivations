module Main where
import Data.Tree(Tree, flatten, unfoldTree)
import Diagrams.Backend.SVG.CmdLine(defaultMain)
import Diagrams.Backend.SVG(SVG)
import Diagrams.Prelude

type TreeNode = (P2, R2)
type Dgm = Diagram SVG R2

main :: IO ()
main = defaultMain $ pad 1.1 $ renderTree buildTree

renderTree :: Tree TreeNode -> Dgm
renderTree = mconcat . flatten . fmap drawBranch

buildTree :: Tree TreeNode
buildTree = unfoldTree branches seed

seed :: TreeNode
seed = (origin, unitY)

drawBranch :: TreeNode -> Dgm
drawBranch (p, v) = place (fromOffsets [v]) p

branches :: TreeNode -> (TreeNode, [TreeNode])
branches   (p, v)
    | magnitude v < 0.05  =  ((p, v), [])
    | otherwise           =  ((p, v), pvs)
    where pvs  = [(p', br (1/7)), (p', br (-1/7))]
          p'   = p .+^ v
          br a = v # scale 0.6 # rotateBy a
