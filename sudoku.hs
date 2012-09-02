import Data.Bits
import Data.List
import Data.Vector ((!), Vector, (//))
import qualified Data.Vector as V
type Grid = Vector PossSet
type PossSet = Int
type Index = Int
type Value = Int
data Region = Row Int | Col Int | Sq Int deriving(Show)
line = "0 0 12 0 0 0 21 0 0 11 0 0 0 7 0 0 0 0 3 17 0 0 9 0 24 0 0 0 0 17 0 0 18 0 0 11 0 0 0 0 0 22 0 0 0 1 13 20 0 0 4 1 2 8 9 0 0 0 3 0 0 0 24 20 0 0 6 0 0 0 0 0 0 22 11 21 22 24 23 0 0 4 10 5 0 0 0 9 18 1 0 0 15 0 0 3 8 0 0 0 11 0 0 7 0 24 6 0 2 23 17 4 0 0 12 0 0 0 0 0 0 0 15 0 0 0 0 0 0 0 17 9 21 0 0 0 15 0 19 0 0 0 0 18 0 0 0 0 16 14 0 0 0 5 0 4 22 11 0 10 0 0 0 16 17 0 0 12 0 1 13 9 25 0 8 0 0 6 0 3 0 18 1 0 0 0 0 14 21 7 0 0 0 9 23 19 0 0 2 0 0 9 0 17 8 0 15 25 0 0 12 0 0 4 0 0 2 0 0 11 20 0 21 0 0 0 13 7 0 0 23 3 0 0 0 0 0 20 0 0 0 0 0 0 10 0 18 0 4 22 13 0 18 0 5 2 0 0 0 0 0 0 4 0 0 3 0 0 0 8 0 1 0 7 23 0 0 0 16 23 0 0 7 0 0 1 25 0 0 5 0 0 0 0 0 24 0 14 0 0 0 0 0 11 25 0 0 12 0 0 0 0 0 23 21 20 0 14 4 0 0 0 0 0 0 8 12 20 19 0 0 0 0 23 0 0 0 0 0 11 24 0 0 0 6 0 0 17 10 0 14 2 0 0 0 0 8 0 19 25 6 16 0 3 9 11 0 5 0 12 0 0 0 20 15 12 17 0 0 0 0 0 5 0 21 18 0 6 0 0 2 9 0 0 24 4 0 10 0 20 2 0 0 1 0 0 0 0 0 3 0 0 0 0 25 19 0 0 21 22 16 0 0 24 0 20 0 0 24 16 0 10 0 0 0 0 0 17 1 0 0 0 23 0 5 18 25 0 3 0 0 8 0 0 14 25 17 0 0 0 24 9 19 5 0 6 0 0 20 0 0 11 23 1 0 0 0 11 25 6 20 1 0 0 7 0 0 16 14 0 0 0 0 10 15 17 12 0 0 21 22 23 0 0 0 21 0 16 0 0 0 8 0 0 18 7 0 24 0 0 0 14 13 0 17 0 7 0 15 0 0 20 0 0 6 0 24 0 2 14 13 0 0 11 3 0 0 5 0 25 3 21 0 10 0 7 25 14 15 19 0 0 0 9 0 22 0 6 0 0 2 0 0 0 0 9 0 0 0 0 18 5 0 0 0 23 19 15 0 10 0 0 1 0 0 0 0 11 0 0 0 16 0 0 20 3 0 24 13 4 0 0 0 17 0 0 0 0 0 25 0 21 12 15 0"
grid = toGrid $ words line

foldlInt :: (a -> b -> Int -> a) -> a -> [b] -> a
foldlInt f init seq = fst $ foldl (\(agg, i) next -> (f agg next i, i+1)) (init, 0) seq

intSqRt :: Int -> Int
intSqRt = truncate . sqrt . fromIntegral

sideLen :: Vector a -> Int
sideLen = intSqRt . V.length

allBits :: Grid -> PossSet
allBits g = (shift 1 $ V.length g) - 1
		
setCell :: Grid -> Index -> Value -> Grid
setCell g i v = eliminate g i $ (allBits g) `clearBit` v

solved :: PossSet -> Bool
solved p = popCount p == 1

solvedG :: Grid -> Index -> Bool
solvedG g i = solved (g!i)

toGrid :: [String] -> Grid
toGrid words = foldlInt reduce empty arr
	where
		arr = map read words :: [Int]
		sidelen = intSqRt $ length $ arr
		bits = (shift 1 sidelen) - 1
		empty = V.replicate (length arr) bits
		reduce g x i = if x==0 then g else setCell g i (x-1)

brothers :: Grid -> Index -> [Index]
brothers g i = [x | r<-intersections g i, x<-cells g r, x /= i]

cells :: Grid -> Region -> [Index]
cells g (Row row) = [row*sl+col | col <- [0..sl-1]]
	where sl = sideLen g
cells g (Col col) = [row*sl+col | row <- [0..sl-1]]
	where sl = sideLen g
cells g (Sq i) = [row*sl+col | row <- [rs..rs+slq-1], col <- [cs..cs+slq-1]]
	where
		sl = sideLen g
		slq = intSqRt sl
		rs = i `div` slq * slq
		cs = i `mod` slq * slq		
	
intersections :: Grid -> Index -> [Region]
intersections g i = 
	let 
		sl = sideLen g
		row = i `div` sl
		col = i `mod` sl
		slq = intSqRt sl
	in [Row row, Col col, Sq $ row `div` slq * slq + col `div` slq]
	
allRegions :: Grid -> [Region]
allRegions g = concat [[Row i, Col i, Sq i] | i <- [0..sideLen g - 1]]

containsVal :: PossSet -> Value -> Bool
containsVal poss value = poss .&. (shift 1 value) /= 0

eliminate :: Grid -> Index -> PossSet -> Grid
eliminate g i elim = 
    let p1 = g!i
        p2 = p1 .&. complement elim
        g2 = g // [(i, p2)]
        cells = brothers g i
        eliminateAll = foldl (\gridAcc x -> eliminate gridAcc x p2) g2 cells
    in  if p2 == p1 then g else if (solved p2) then eliminateAll else g2

lr :: Grid -> Grid
lr g = foldl lr' g $ allRegions g
    where
      lr' :: Grid -> Region -> Grid
      lr' g region = foldl (lr'' region) g [0..sideLen g - 1]
      lr'' :: Region -> Grid -> Value -> Grid
      lr'' region g value = if lone then setCell g index value else g
        where
          containsValue :: Index -> Bool
          containsValue i = g!i `containsVal` value
          containers = filter containsValue $ cells g region
          lone = length containers == 1
          index = head containers
          
si :: Grid -> Grid
si g = foldl (\g (i,r) -> si' g r $ g!i) g [(i,r) | i <- [0..V.length g - 1], r <- intersections g i]
    where
      si' :: Grid -> Region -> PossSet -> Grid
      si' g region poss = if length limitedToPoss == popCount poss then g' else g
          where
            indexes = cells g region
            isLimitedToPoss i = g!i .|. poss == poss
            (limitedToPoss, notLimitedToPoss) = partition isLimitedToPoss indexes
            g' = foldl (\g i -> eliminate g i poss) g notLimitedToPoss

isConsistent :: Grid -> Bool            
isConsistent g = not $ V.elem 0 g

te :: (Grid -> Grid) -> Grid -> Grid
te f g = foldl te' g $ filter (not . solvedG g) [0..V.length g - 1]
    where
        te' :: Grid -> Index -> Grid
        te' g i = foldl te'' g $ filter (testBit poss) [0..sideLen g - 1]
            where 
                poss = g!i
                te'' :: Grid -> Value -> Grid
                te'' g v = if isConsistent clone then g else g'
                    where 
                        clone = f $ setCell g i v
                        g' = eliminate g i $ bit v
      

run :: Eq a => (a -> a) -> a -> a
run f a = if a == b then a else run f b
    where b = f a
    
runAll :: Eq a => [(a -> a)] -> a -> a
runAll fs = if null fs then id else run $ (head fs) . runAll (tail fs)

runSi :: Grid -> Grid
runSi = runAll [si, lr]

teLr = te $ run lr
runTe = runAll [teLr, si, lr]

elimCount :: Grid -> Int
elimCount g = V.foldl (\i p -> i - popCount p) 15625 g

x = runSi grid
y = runTe grid
