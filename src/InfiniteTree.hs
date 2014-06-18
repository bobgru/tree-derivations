import Data.Tree(Tree(..), flatten, unfoldTree)
import Diagrams.Backend.SVG.CmdLine(defaultMain)
import Diagrams.Prelude

main       = defaultMain $ pad 1.1 $ renderTree buildTree
renderTree = mconcat . fmap drawBranch . flatten . prune
buildTree  = unfoldTree branches seed
seed       = (origin, unitY)
drawBranch (p, v) = place (fromOffsets [v]) p
branches   (p, v) = ((p, v), [(q, br (1/7)), (q, br (-1/7))])
    where q = p .+^ v; br a = v # scale 0.6 # rotateBy a
prune (Node n@(_, v) pvs) | magnitude v > 0.05 = Node n (map prune pvs)
                          | otherwise          = Node n []
