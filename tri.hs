import System.IO
import Debug.Trace
import qualified Data.Set as Set
import Data.Function
import Control.Monad
import System.Random
import qualified Text.JSON as JS
import qualified Data.List as L
import qualified Data.Matrix as Mx
import qualified Numeric.LinearAlgebra.Data as D
import qualified Numeric.LinearAlgebra.Algorithms as Ag
import qualified Data.Vector as V
import qualified Data.Tuple as T
import qualified Data.Map.Strict as M

detailed = False 
type Pt = [Double]
--always 5 elements   
type Pt2 = [Double]
--always 6 elements

inf :: Pt
inf = [0.0,0.0,0.0,0.0,1.0]

inf2 :: Pt2
inf2 = [0.0,0.0,0.0,0.0,1.0,1.0]

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
contain x y = (dot x y) >= -0.000001
   
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
     
prettyT :: Tetra -> String
prettyT t = foldr (\x y -> show x++"\n"++y) "" $ M.toList t
 
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
addl (x:xs) tree  = trace str1 $ addl xs newtree where 
    str1 = if detailed then ("<card>"++show x++"</card>") else ""
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
          vts = tol.vertices $ ct
          
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
    |M.member (a,b) tree = error $ "wtf3!!!!!!!!!!!!!!!!!"++show (u,a,b,c) ++ "ugh!!!!!!!!!!!"++show (tree M.! (a,b))
    |otherwise = M.insert (a,b) (c,inf,u,-1,3,-1) tree

addtri2 :: Int -> (Pt,Pt,Pt) -> Triag -> Triag
addtri2 u (a,b,c) tree
    |M.member (c,b) tree = M.adjust (modedge 1 u a) (c,b) tree
    |M.member (b,c) tree = error $ "wtf1!!!!!!!!!!!!!!!!!"++show (u,a,b,c) ++ "ugh!!!!!!!!!!!"++show (tree M.! (b,c))
    |otherwise = M.insert (b,c) (a,inf,u,-1,1,-1) tree
   
addtri3 :: Int -> (Pt,Pt,Pt) -> Triag -> Triag
addtri3 u (a,b,c) tree
    |M.member (a,c) tree = M.adjust (modedge 2 u b) (a,c) tree
    |M.member (c,a) tree = error $ "wtf2!!!!!!!!!!!!!!!!!"++show (u,a,b,c) ++  "ugh!!!!!!!!!!!"++show (tree M.! (c,a))
    |otherwise = M.insert (c,a) (b,inf,u,-1,2,-1) tree
    
addtri :: Card -> Triag -> Triag
addtri (Card a _ _ _ _ _ x _) = addtri1 a u.addtri2 a u.addtri3 a u where u=outet x

origin :: Pt
origin = [1.0,0.0,0.0,0.0,0.0]     
origin2 :: Pt2
origin2 = [1.0,0.0,0.0,0.0,0.0,0.0]     
    
totet :: Pt -> (Pt,Pt,Pt) -> Tet
--gets positive orientation
totet x (a,b,c) = (x,a,b,c)
   
normal :: D.Vector Double -> D.Vector Double
normal x = x / (D.scalar $ Ag.pnorm Ag.PNorm2 x)
        
getcir :: Tet -> Pt
getcir (a,b,c,d) = [x0,x1,x2,x3,x4] where
    mata :: Mx.Matrix Double
    mata = Mx.fromLists [a,b,c,d,inf]
    x0 = (*(-1)).Mx.detLU.(Mx.minorMatrix 5 1) $ mata
    x1 = Mx.detLU.(Mx.minorMatrix 5 2) $ mata
    x2 = (*(-1)).Mx.detLU.(Mx.minorMatrix 5 3) $ mata
    x3 = Mx.detLU.(Mx.minorMatrix 5 4) $ mata
    x4 = (*(-1)).Mx.detLU.(Mx.minorMatrix 5 5) $ mata

getcir0 :: (Pt2,Pt2) -> Pt2
getcir0 (a,b) = result where
    [1.0,x1,x2,x3,x4,x5]=a
    [1.0,y1,y2,y3,y4,y5]=b
    s = dot [x1,x2,x3,x4] [y1,y2,y3,y4] 
    result = [(-1)*s,x1+y1,x2+y2,x3+y3,x4+y4,-1.0]



getcir1 :: (Pt2,Pt2,Pt2) -> Pt2 
getcir1 (a,b,c) = result where 
    mata :: D.Matrix Double
    mata = D.fromLists [a,b,c]
    norvecs :: [D.Vector Double]
    norvecs = Ag.nullspacePrec 1 $ D.subMatrix (0,0) (3,5) mata
    la :: D.Matrix Double
    lb :: D.Matrix Double
    lc :: D.Matrix Double
    la = convnormal $ norvecs!!0
    lb = convnormal $ norvecs!!1
    lc = D.assoc (1,6) 0 [((0,5),1)]
    matb :: D.Matrix Double
    matb = ((mata D.=== la) D.=== lb) D.=== lc
    resb :: D.Vector Double
    resb = D.assoc 6 0 [(5,-1.0)]
    result :: [Double]
    result = D.toList $ (D.toColumns $ Ag.linearSolve matb (D.asColumn resb))!!0

convnormal :: D.Vector Double -> D.Matrix Double
convnormal v = (D.asRow).(D.fromList) $ [0,(-1)*(v D.! 1),(-1)*(v D.! 2),(-1)*(v D.! 3),(-1)*(v D.! 4),2*(v D.! 0)]


-- want (2*x5,-x1,-x2,-x3,-x4) which is center to be on the plane
-- so dot that with n has to be 0
-- so x0*0+x1*(-n1)+x2*(-n2)+x3*(-n3)+x4*(-n4)+x5*(2*n0)
--remember there is no n5
--
getcir2 :: Tet -> Pt2
getcir2 (a,b,c,d) = result where
    mata :: D.Matrix Double
    mata = D.fromLists [a,b,c,d]
    norvecs :: [D.Vector Double]
    norvecs = Ag.nullspacePrec 1 $ D.subMatrix (0,0) (4,5) mata
    la :: D.Matrix Double
    la = convnormal $ norvecs!!0
    lb :: D.Matrix Double
    lb = D.assoc (1,6) 0 [((0,5),1)]
    matb :: D.Matrix Double
    matb = (mata D.=== la) D.=== lb
    resb :: D.Vector Double
    resb = D.assoc 6 0 [(5,-1.0)]
    result :: [Double]
    result = D.toList $ (D.toColumns $ Ag.linearSolve matb (D.asColumn resb))!!0
--note DO NOT use any of these three with Inf point and expect it to work!

access :: Triag -> Edge -> (Pt, Pt, Int, Int,Int,Int)
access tg e 
    |M.member e tg = tg M.! e 
    |M.member (T.swap e) tg = tg M.! (T.swap e)
    |otherwise = error "access error"
   
pushout :: Int -> (Pt, Pt, Int, Int, Int, Int) -> (Int,Int)
pushout x (a,b,c,d,e,f)
    |(d == -1) || (c == -1)  = trace ("start pushout error"++show (x,a,b,c,d,e,f)) $ error "undefined thing"
    |x == d = (c,e)
    |x == c = (d,f)
    |otherwise = trace ("start pushout error"++show (x,a,b,c,d,e,f)) $ error "can't pushout"
  
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
        str1 = ""--("\na1b1:"++show (w,v)++"\n")
        str2 = ""--("\na1b1:"++show (u,w)++"\n")
        str3 = ""--("\na1b1:"++show (u,v)++"\n")
        (a1,b1)=trace str1 (pushout n $ access triag (w,v))
        (a2,b2)=trace str2 (pushout n $ access triag (u,w))
        (a3,b3)=trace str3 (pushout n $ access triag (u,v))
        --the pointers to the other ones
        oi = 16*b1+4*b2+b3+b
        newcard = (Card n k a1 a2 a3 oi x c)

fadd :: Pt -> Card -> [Card] -> [Card]
fadd cur ct xtl = if (contain cur (circumcir ct)) then (ct:xtl) else xtl
   
addkey :: Int -> Tetra -> ((Int,Int),Tet) -> [Card] -> [Card] 
addkey count tree (k,x) xl = (Card n (snd k) (-1) (-1) (-1) oi x c):xl where 
    n = count+length xl
    oi = 64 * (getoi (tree M.! (snd k)) (fst k))
    c = getcir x

convtoPt :: (Double,Double,Double) -> Pt
convtoPt (x,y,z) = [1,x,y,z,x^2+y^2+z^2]
 
lshow :: (Show a) => [a] -> String
lshow = foldr (\x y -> show x++"\n"++y) ""

bow_wat :: [(Double,Double,Double)] -> Tetra -> Int -> Tetra
bow_wat [] tree count = tree
bow_wat (p:ps) tree count= bow_wat ps utree newcount where
    current = convtoPt p
    str1 = if detailed then ("---tree"++prettyT tree++"---current"++show current++"---") else ""
    str2 = if detailed then ("---poorlist"++lshow poorlist++"---") else ""
    str3 = if detailed then ("---boundary"++show boundary++"---") else ""
    str4 = if detailed then ("---listcard"++lshow listcards++"---") else ""
    str5 = if detailed then ("---trig"++show trig++"---") else ""
    poorlist :: [Card]
    poorlist = trace str1 $ M.foldr' (fadd current) [] tree
    --fadd only adds the tet if it infringes
    --list to be deleted
    boundary :: [(Int,Int)]
    boundary = trace str2 $ foldr getbound [] poorlist
    --boundary of deleted
    faces :: [(Pt,Pt,Pt)]
    faces = trace str3 $ map (getface tree) boundary
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
    trig  = trace str4 $ foldr addtri M.empty listcards
    
    kus :: [Card]
    kus = trace str5 $ map (mkpoint trig) listcards
    
    ttree :: Tetra
    ttree = dell (map getself poorlist) tree
    
    utree :: Tetra
    utree = addl kus ttree
 
backpt :: Pt
backpt = [1.0,-30.0,-30.0,-30.0,2700.0] 
xaxis :: Pt
xaxis = [1.0,30.0,0.0,0.0,900.0]
yaxis :: Pt
yaxis = [1.0,0.0,30.0,0.0,900.0]
zaxis :: Pt
zaxis = [1.0,0.0,0.0,30.0,900.0]

adapt :: Bool -> D.Vector Double-> [Double]
adapt sw v
    |abs (v D.! 0) < 0.000001 = D.toList v
    |(v D.! 0 > 0) && sw = D.toList v
    |(v D.! 0 < 0) && sw = D.toList $ v * (D.scalar (-1))
    |(v D.! 0 < 0) && (not sw) = D.toList v
    |(v D.! 0 > 0) && (not sw) = D.toList $ v * (D.scalar (-1))

plane2 :: Bool -> (Pt2,Pt2,Pt2) -> Pt2
plane2 sw (a,b,c) = result where
    v :: D.Vector Double
    v = Ag.nullVector $ D.fromLists [a,b,c,[0,0,0,0,1,0::Double],[0,0,0,0,0,1::Double]]
    result = adapt sw v

starting :: Tetra
starting = M.fromList [(0,a),(1,b),(2,c),(3,d),(4,e)] where
    a :: Card
    a = Card 0 1 2 3 4 0 (backpt,xaxis,yaxis,zaxis) (getcir (backpt,xaxis,yaxis,zaxis))
    b :: Card
    b = Card 1 0 2 4 3 62 (inf,xaxis,zaxis,yaxis) (getcir (inf,xaxis,zaxis,yaxis))
    c :: Card
    c = Card 2 0 3 4 1 85 (inf,yaxis,zaxis,backpt) (getcir (inf,yaxis,zaxis,backpt))
    d :: Card
    d = Card 3 0 2 1 4 158 (inf,xaxis,backpt,zaxis) (getcir (inf,xaxis,backpt,zaxis))
    e :: Card
    e = Card 4 0 2 3 1 238 (inf,xaxis,yaxis,backpt) (getcir (inf,xaxis,yaxis,backpt))
   

data Vertex = Vex (Double,Double,Double) deriving (Ord,Show,Eq)
type Threegraph = Set.Set (Vertex,Vertex)

tovertex :: Pt -> Vertex
tovertex [a,b,c,d,e] = Vex (b/a,c/a,d/a)
tovertex _ = error "not a vertex"

pushon :: [(Pt,Pt)] -> Threegraph
pushon [] = Set.empty
pushon (x:xs) = Set.insert (tovertex.fst $ x,tovertex.snd $ x) $ pushon xs

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

keep :: Card -> Bool
keep card
    |elem inf xl = False 
    |elem xaxis xl = False
    |elem yaxis xl = False
    |elem zaxis xl = False
    |elem backpt xl = False
    |otherwise = True
    where x = vertices card
          xl = [lemon 0 x,lemon 1 x,lemon 2 x,lemon 3 x]

ptup :: Pt -> Pt2
ptup [1.0,b,c,d,e] = [1.0,b,c,d,w,b^2+c^2+d^2+w^2] where w = surface (b,c,d)
 
pushup :: Tetra -> Card -> Card
pushup tetra cardc = cardd where
    Card xa xb xc xd xe xf xg xh = cardc
    xb2 = if (M.member xb tetra) then xb else (-1)
    xc2 = if (M.member xc tetra) then xc else (-1)
    xd2 = if (M.member xd tetra) then xd else (-1)
    xe2 = if (M.member xe tetra) then xe else (-1)
    
    (a,b,c,d) = xg 
    a2 = ptup a
    b2 = ptup b
    c2 = ptup c
    d2 = ptup d
    cir2 = getcir2 (a2,b2,c2,d2)    
    cardd = Card xa xb2 xc2 xd2 xe2 xf (a2,b2,c2,d2) cir2 
         
convup :: Tetra -> Tetra 
convup tetra = M.map (pushup tetra2) tetra2 where tetra2 = M.filter keep tetra

data Truegraph = Truegraph {truevertex :: Set.Set Pt2,trueedge :: Set.Set (Pt2,Pt2)} 

improvetetra :: Tetra -> Tetra
improvetetra = undefined

failedge :: (Pt2,Pt2) -> Set.Set Pt2 -> Bool
failedge (p1,p2) setp = undefined --Set.foldr (\p t -> True|| t)

impedges :: Tetra -> Tetra
impedges tetra = newtetra where
   newtetra = undefined 

------------------------------------------------------------------------------------------------------------------------
surface :: (Double,Double,Double) -> Double
surface (x,y,z) = 0 --x^4+y^4+z^4

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
main :: IO ()
main = do
    let n_pts = 4 
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
    let model2 = convup model
    putStrLn $ prettyT model2
    --print.toJSONedges.tograph $ model 
    writeFile "test.json" (toJSONedges.tograph $ model)

    return ()

    
