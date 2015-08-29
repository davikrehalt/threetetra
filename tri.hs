import System.IO
import Debug.Trace
import qualified Data.Set as Set
import Data.Function
import Control.Monad
import System.Random
import qualified Text.JSON as JS
import qualified Data.List as L
import qualified Data.Matrix as Mx
import qualified Data.Vector as V
import qualified Data.Tuple as T
import qualified Data.Map.Strict as M

surface :: (Double,Double,Double) -> Double
surface (x,y,z) = cos x 

fdiv :: Int -> Int -> Double
fdiv = (/) `on` fromIntegral

ntotal :: Int
-- division number
ntotal = 10

mtotal :: Double
mtotal = fromIntegral ntotal

posl :: [(Int,Int,Int)]
posl=[(x,y,z) | x <- [0..ntotal-1], y <- [0..ntotal-1], z <- [0..ntotal-1]]

posgl :: [(Double,Double,Double)]
posgl=[(fdiv (2*x+1) (2*ntotal),fdiv (2*y+1) (2*ntotal), fdiv (2*z+1) (2*ntotal)) | x <- [0..ntotal-1], y <- [0..ntotal-1], z <- [0..ntotal-1]]

grad ::  V.Vector (V.Vector (V.Vector Double)) -> (Int,Int,Int) -> (Double,Double,Double)
--gradient
--input from 0 to n-1 each
grad l (x,y,z) = (0.25*(e+f+g+h-a-b-c-d)*mtotal,0.25*(c+d+g+h-a-b-e-f)*mtotal,0.25*(b+d+f+h-a-c-e-g)*mtotal) where

    a=l V.! x     V.! y     V.! z
    b=l V.! x     V.! y     V.!(z+1)
    c=l V.! x     V.! (y+1) V.!z
    d=l V.! x     V.! (y+1) V.!(z+1)
    e=l V.! (x+1) V.! y     V.!z
    f=l V.! (x+1) V.! y     V.!(z+1)
    g=l V.! (x+1) V.! (y+1) V.!z
    h=l V.! (x+1) V.! (y+1) V.!(z+1)

density :: (Double,Double,Double) -> Double
density (x, y, z)= sqrt $ (1+x^2+y^2+z^2)

fimage :: ((Double,Double,Double) -> Double) -> V.Vector (V.Vector (V.Vector Double))
fimage sf = V.generate (ntotal+1) (\x -> V.generate (ntotal+1) (\y -> V.generate (ntotal+1) (\z -> sf (fdiv x ntotal,fdiv y ntotal, fdiv z ntotal)))) 

psum :: [Double] -> [Double]
psum = scanl1 (+)

insertP :: [Double] -> [Double] -> [Bool]
insertP [] yl = [False | x <- yl]
insertP xl [] = trace (show xl) (error "out of place points")
insertP (x:xs) (y:ys) 
    |x > y = False:insertP (x:xs) ys
    |x <= y = True:tail (insertP xs (y:ys))

orderpt :: (Double,Double,Double) -> (Double,Double,Double) -> Ordering
orderpt (x,y,z) (a,b,c)
    | x+y+z > a+b+c = GT
    | x+y+z < a+b+c = LT
    | x > a = GT
    | x < a = LT
    | y > a = GT
    | y < a = LT
    |otherwise = EQ

fil :: [Bool] -> [a] -> [a]
--actual filtering
fil [] [] = []
fil (x:xs) (y:ys)
    |x = y:fil xs ys
    |otherwise = fil xs ys
 
pointslist :: [Bool] -> [(Double,Double,Double)] -> [(Double,Double,Double)]
--filters the points and sorts them
pointslist bl pl = L.sortBy orderpt $ fil bl pl

type Pt = [Double]
--always 5 elements   

inf :: Pt
inf = [0.0,0.0,0.0,0.0,1.0]

type Tet=(Pt,Pt,Pt,Pt)
--(a0,a1,a2,a3)

dot :: Pt -> Pt -> Double 
dot x y = sum $ zipWith (*) x y


lemon :: Int -> (a,a,a,a) -> a
lemon 0 (a,b,c,d) = a
lemon 1 (a,b,c,d) = b
lemon 2 (a,b,c,d) = c
lemon 3 (a,b,c,d) = d

contain :: Pt -> Pt -> Bool
contain x y = (dot x y) >= 0
    
convtoPt :: (Double,Double,Double) -> Pt
convtoPt (x,y,z) = [1,x,y,z,x^2+y^2+z^2]
    
merge :: [(Int,Int)] -> [(Int,Int)] ->[(Int,Int)]
merge [] xl = xl
merge (x:xs) xl
    |elem y xl = merge xs (L.delete y xl)
    |otherwise = merge xs (x:xl)
    where y = T.swap x
    
data Card = Card {
    getself::Int,
    neigh0::Int,
    neigh1::Int,
    neigh2::Int,
    neigh3::Int,
    connections::Int,
    vertices::Tet,
    circumcir::Pt} deriving (Show)

--self,neighbors,orientation,self,circle
--Orientation is like this 

direct :: Card -> (Int,Int,Int,Int)
direct ct = ((quot k 64) `mod` 4,(quot k 16) `mod` 4,(quot k 4) `mod` 4,(k) `mod` 4) where k = connections ct

-- 0 is side without zero ,i.e. (1,2,3)
--first element of tuple is for the first Int occurence after self
--for exampe (0,_,_,_) means the zeroth neighbor has the side without zero touching us


getbound :: Card -> [(Int,Int)] -> [(Int,Int)]
getbound (Card a b c d e _ _ _) = merge [(a,b),(a,c),(a,d),(a,e)] 
  
tol :: Tet -> [Pt]
tol (a,b,c,d) = [a,b,c,d]
     
   
type Tetra = M.Map Int Card
   
dell :: [Int] -> Tetra -> Tetra
dell [] tree = tree
dell (x:xs) tree  = dell xs (M.delete x tree)
    
updatecard :: Int -> Int -> Card -> Card
updatecard 0 x (Card a b c d e f g h) = Card a x c d e f1 g h where
    f1 = f `mod` 64
updatecard 1 x (Card a b c d e f g h) = Card a b x d e f1 g h where
    f1 = f - 16*((quot f 16) `mod` 4)
updatecard 2 x (Card a b c d e f g h) = Card a b c x e f1 g h where
    f1 = f - 4*((quot f 4) `mod` 4)
updatecard 3 x (Card a b c d e f g h) = Card a b c d x f1 g h where
    f1 = f - (f `mod` 4)

addl :: [Card] -> Tetra -> Tetra
addl [] tree = tree
addl (x:xs) tree  = trace ("<card>"++show x++"</card>") $ addl xs newtree where 
    a = getself x
    b = neigh0 x
    f = connections x 
    bb = quot f 64 
    oldtree = M.adjust (updatecard bb a) b tree
    newtree = M.insert a x oldtree
    
super :: Tetra
super = undefined

getface :: Tetra -> (Int,Int) -> (Pt,Pt,Pt)
getface tree (x,y)
--outwards from x
    |neigh0 ct == y = (vts!!1,vts!!2,vts!!3)
    |neigh1 ct == y = (vts!!3,vts!!2,vts!!0)
    |neigh2 ct == y = (vts!!3,vts!!0,vts!!1)
    |neigh3 ct == y = (vts!!0,vts!!2,vts!!1)
    |otherwise = error "not touching"
    where ct = tree M.! x
          vts = tol.vertices $  ct
          
type Edge = (Pt,Pt)
type Triag = M.Map Edge (Pt,Pt,Int,Int,Int,Int)
--second is a pseudo edgoe
--two points on each side, who they belong to and the orientation
--the orientation is which one the side vertex is (which number) 
         
modedge :: Int -> Int -> Pt -> (Pt,Pt,Int,Int,Int,Int) -> (Pt,Pt,Int,Int,Int,Int)
modedge k u x (a,b,c,d,e,f) = (a,x,c,u,e,k)
   
outet :: Tet -> (Pt,Pt,Pt)
outet (a,b,c,d) = (b,c,d)
   
addtri1 :: Int -> (Pt,Pt,Pt) -> Triag -> Triag
addtri1 u (a,b,c) tree
    |M.member (b,a) tree = M.adjust (modedge 3 u c) (b,a) tree
    |otherwise = M.insert (a,b) (c,inf,u,-1,3,-1) tree

addtri2 :: Int -> (Pt,Pt,Pt) -> Triag -> Triag
addtri2 u (a,b,c) tree
    |M.member (c,b) tree = M.adjust (modedge 1 u a) (c,b) tree
    |otherwise = M.insert (b,c) (a,inf,u,-1,1,-1) tree
   
addtri3 :: Int -> (Pt,Pt,Pt) -> Triag -> Triag
addtri3 u (a,b,c) tree
    |M.member (a,c) tree = M.adjust (modedge 2 u b) (a,c) tree
    |otherwise = M.insert (c,a) (b,inf,u,-1,2,-1) tree
    
addtri :: Card -> Triag -> Triag
addtri (Card a _ _ _ _ _ x _) = addtri1 a u.addtri2 a u.addtri3 a u where u=outet x

     
    
totet :: Pt -> (Pt,Pt,Pt) -> Tet
--gets positive orientation
totet x (a,b,c) = (x,a,b,c)
    
getcir :: Tet -> Pt
getcir (a,b,c,d) = [x0,x1,x2,x3,x4] where
    mata :: Mx.Matrix Double
    mata = Mx.fromLists [a,b,c,d,inf]
    x0 = (*(-1)).Mx.detLU.(Mx.minorMatrix 5 1) $ mata
    x1 = Mx.detLU.(Mx.minorMatrix 5 2) $ mata
    x2 = (*(-1)).Mx.detLU.(Mx.minorMatrix 5 3) $ mata
    x3 = Mx.detLU.(Mx.minorMatrix 5 4) $ mata
    x4 = (*(-1)).Mx.detLU.(Mx.minorMatrix 5 5) $ mata
    
access :: Triag -> Edge -> (Pt, Pt, Int, Int,Int,Int)
access tg e 
    |M.member e tg = tg M.! e 
    |M.member (T.swap e) tg = tg M.! (T.swap e)
    |otherwise = error "access error"
   
pushout :: Int -> (Pt, Pt, Int, Int, Int, Int) -> (Int,Int)
pushout x (a,b,c,d,e,f)
    |x == d = (c,e)
    |x == c = (d,f)
    |otherwise = trace (show (x,a,b,c,d)) $ error "can't pushout"
  
getoi :: Card -> Int -> Int
getoi x n
    |a0 == n = 0
    |a1 == n = 1
    |a2 == n = 2
    |a3 == n = 3
    |otherwise = error "wtf"
    where (Card _ a0 a1 a2 a3 _ _ _) = x
   
mkpoint :: Triag -> Card -> Card
mkpoint triag card = newcard where
        (Card n k _ _ _ b x c) = card
        (_,u,v,w) = x
        (a1,b1)=pushout n $ access triag (w,v)
        (a2,b2)=pushout n $ access triag (u,w)
        (a3,b3)=pushout n $ access triag (u,v)
        --the pointers to the other ones
        oi = 16*b1+4*b2+b3+b
        newcard = (Card n k a1 a2 a3 oi x c)

fadd :: Pt -> Card -> [Card] -> [Card]
fadd cur ct xtl = if (contain cur (circumcir ct)) then (ct:xtl) else xtl
    
testpoint :: Pt
testpoint = [1.0,0.3,0.5,0.3,0.43] 
   
addkey :: Int -> Tetra -> ((Int,Int),Tet) -> [Card] -> [Card] 
addkey count tree (k,x) xl = (Card n (snd k) (-1) (-1) (-1) oi x c):xl where 
    n = count+length xl
    oi = 64 * (getoi (tree M.! (snd k)) (fst k))
    c = getcir x


 
bow_wat :: [(Double,Double,Double)] -> Tetra -> Int -> Tetra
bow_wat [] tree count = tree
bow_wat (p:ps) tree count= bow_wat ps utree newcount where
    current = convtoPt p
   
    poorlist :: [Card]
    poorlist = M.foldr' (fadd current) [] tree
    --list to be deleted
    boundary :: [(Int,Int)]
    boundary = foldr getbound [] poorlist
    --boundary of deleted
    faces :: [(Pt,Pt,Pt)]
    faces = trace ("---boundary"++show boundary++"---") $ map (getface tree) boundary
    --outwards from hole
    newlist :: [Tet]
    newlist = map (totet current) faces
    newlist2 :: [((Int,Int),Tet)]
    newlist2 = zip boundary newlist
    --correctly oriented
        --keys added
    newcount = count + length newlist
    
    listcards :: [Card]
    listcards = foldr (addkey count tree) [] newlist2
    --list of cards b/f
    
    trig :: Triag
    trig  = foldr addtri M.empty listcards
    
    kus :: [Card]
    kus = map (mkpoint trig) listcards
    
    ttree :: Tetra
    ttree = dell (map getself poorlist) tree
    
    utree :: Tetra
    utree = addl kus ttree
    
origin :: Pt
origin =[1.0,0.0,0.0,0.0,0.0]
backpt :: Pt
backpt = [1.0,-1.0,-1.0,-1.0,3.0]
xaxis :: Pt
xaxis = [1.0,2.0,0.0,0.0,4.0]
yaxis :: Pt
yaxis = [1.0,0.0,2.0,0.0,4.0]
zaxis :: Pt
zaxis = [1.0,0.0,0.0,2.0,4.0]

starting :: Tetra
starting = M.fromList [(0,a),(1,b),(2,c),(3,d),(4,e)] where
    a :: Card
    a = (Card 0 1 2 3 4 0 (backpt,xaxis,yaxis,zaxis) (getcir (backpt,xaxis,yaxis,zaxis)))
    b :: Card
    b = (Card 1 0 2 4 3 62 (inf,xaxis,zaxis,yaxis) (getcir (inf,xaxis,zaxis,yaxis)))
    c :: Card
    c = (Card 2 0 3 4 1 85 (inf,yaxis,zaxis,backpt) (getcir (inf,yaxis,zaxis,backpt)))
    d :: Card
    d = (Card 3 0 2 1 4 158 (inf,xaxis,backpt,zaxis) (getcir (inf,xaxis,backpt,zaxis)))
    e :: Card
    e = (Card 4 0 2 3 1 238 (inf,xaxis,yaxis,backpt) (getcir (inf,xaxis,yaxis,backpt)))
   

data Vertex = Vex (Double,Double,Double) deriving (Ord,Show,Eq)
type Threegraph = Set.Set (Vertex,Vertex)

tovertex :: Pt -> Vertex
tovertex [a,b,c,d,e] = Vex (b/a,c/a,d/a)
tovertex _ = error "not a vertex"

pushon :: [(Pt,Pt)] -> Threegraph
pushon [] = Set.empty
pushon (x:xs) 
    |snd x == inf = Set.empty
    |fst x == inf = Set.empty
    |snd x == xaxis = Set.empty
    |fst x == xaxis = Set.empty
    |snd x == yaxis = Set.empty
    |fst x == yaxis = Set.empty
    |snd x == zaxis = Set.empty
    |fst x == zaxis = Set.empty
    |snd x == backpt = Set.empty
    |fst x == backpt = Set.empty
    |otherwise= Set.insert (tovertex.fst $ x,tovertex.snd $ x) $ pushon xs

cardset :: Card -> Threegraph
cardset (Card _ _ _ _ _ _ (a,b,c,d) _) = someset where
    someset = pushon [(a,b),(b,a),(a,c),(c,a),(a,d),(d,a),(b,c),(c,b),(b,d),(d,b),(c,d),(d,c)]

addtothreegraph :: Card -> Threegraph -> Threegraph
addtothreegraph c oldset = newset where
    newset = Set.union oldset $ cardset c

tograph :: Tetra -> Threegraph
tograph = M.foldr addtothreegraph Set.empty 

stripVex :: (Vertex,Vertex) -> ((Double,Double,Double),(Double,Double,Double))
stripVex (Vex (a,b,c),Vex (x,y,z)) = ((a,b,c),(x,y,z))

toJSONedges :: Threegraph -> String
toJSONedges graph = JS.encode $ map stripVex (Set.toList graph)

main :: IO ()
main = do
    let n_pts=20
    let n = ntotal
    g <- newStdGen
    let randx = L.sort $ take n_pts (randoms g :: [Double]) 
    -- random doubles
    let pdist = fmap (density.grad (fimage surface)) posl 
    -- distribution
    let distro = let s = L.sum pdist in fmap (/s) pdist
    -- put the random stuff
    let binlist = insertP randx $ psum distro
    --put it to points
    g2 <- newStdGen
    let randy = take (3*n^3) (randoms g2 :: [Double])
    let f x y z = (+) (fdiv x n).(*(fdiv 1 n)) $ randy!!(3*(n^2*x+n*y+z))
    let g x y z = (+) (fdiv y n).(*(fdiv 1 n)) $ randy!!(3*(n^2*x+n*y+z)+1)
    let h x y z = (+) (fdiv z n).(*(fdiv 1 n)) $ randy!!(3*(n^2*x+n*y+z)+2)
    let posglr=[(f x y z,g x y z,h x y z)| x <- [0..n-1], y <- [0..n-1], z <- [0..n-1]]
    let pts = pointslist binlist posglr 
    let model = bow_wat pts starting 5 
    print.toJSONedges.tograph $ model
    writeFile "test.json" (toJSONedges.tograph $ model)

    return ()

    
