import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Monoid

data Waiting
data Active
data Thm

addW :: Double -> [Thm] -> Waiting -> Waiting
addW = undefined

removeW :: Waiting -> Maybe (Waiting, (Double, Thm))
removeW = undefined

addA :: Thm -> Active -> (Active, [Thm])
addA = undefined

initThms :: [Thm]
initThms = undefined

initW :: Waiting
initW = undefined

initA :: Active
initA = undefined

waitings1 :: [Waiting]
waitings1 = initW : zipWith3 addW dists deduced waitings1

adeduced :: ([Active], [[Thm]])
adeduced = unzip (zipWith addA pulled theActives)
  where theActives = initA : fst adeduced

actives :: [Active]
actives = fst adeduced

deduced :: [[Thm]]
deduced = initThms : snd adeduced

wpulled :: ([Waiting], [(Double, Thm)])
wpulled = unzip $ mapMaybe removeW waitings1

dpulled :: ([Double], [Thm])
dpulled = unzip (snd wpulled)

waitings2 :: [Waiting]
waitings2 = fst wpulled

pulled :: [Thm]
pulled = snd dpulled

dists :: [Double]
dists = fst dpulled

accumulateL :: Monoid m => [m] -> [m]
accumulateL = loop mempty
  where loop _ []       = []
        loop acc (x:xs) = acc : loop (acc `mappend` x) xs


preview :: s -> [State s a] -> [(a,s)]
preview _ []     = []
preview s (x:xs) = runState x s : preview s xs

test s = let foo = (1,s) : preview s (map (return . fst) foo) in foo
