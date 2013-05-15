{-# LANGUAGE TemplateHaskell #-}
module DocSnap.VersionControl.Merge
  ( compose
  , merge
  , Edit(..)
  , PackedEdit
  ) where

import System.IO
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import Test.QuickCheck.All
import Data.Ord
import Data.List
import Debug.Trace


import Control.Monad

compose = seqMerge
merge = parMerge

-- konstruktorok, amelyek üres token esetén eldobják a fejelemet
_P 0 rest = rest
_P n rest = P n:rest

_R 0 rest = rest
_R n rest = R n:rest

_I "" rest = rest
_I s rest = I s:rest

type PackedEdit = Edit
  
data Edit =
    I String
  | P Int
  | R Int
  deriving (Show, Eq)  
--------------------------------------------------------------------------------
-- | A szekvenciális összefűzés algoritmusa
--összesen 3*3=9 kombináció lehet
seqMerge:: [PackedEdit] -> [PackedEdit] -> [PackedEdit]
--ha n karaktert eddig töröltünk, akkor az ezután is törölve lesz (3 kombináció)
seqMerge x y = flatten $ seqMergeNew x y

--ha valamelyik üres, akkor at egyszerűen kihagyjuk
seqMergeNew [] r = r
seqMergeNew r [] = r

seqMergeNew (R n:ls) r = R n: seqMergeNew ls r 
--bármit csináltunk eddig, ha ezután ide beszúrunk, akkor azt szúrjuk be (2 kombináció)
--mert az (R,I) esetet az előzőnél kezeltük 
seqMergeNew l (I s:rs) = I s: seqMergeNew l rs


--ha eddig megőriztünk valamit, és ezután is megőrizzük, akkor azt megőrizzük (1 kombináció)
seqMergeNew (P n:ls) (P m:rs) = P k: seqMergeNew (_P (n-k) ls) (_P (m-k) rs) 
  where k = min n m
  
--ha valamit eddig megőriztünk, de ezután töröljük, akkor azt végülis töröljük (1 kombináció)
seqMergeNew (P n:ls) (R m:rs) = R k: seqMergeNew (_P (n-k) ls) (_R (m-k) rs)
  where k = min n m
  
--ha beszúrunk valamit, és megőrizzük, akkor azt végülis beszúrtuk (1 kombináció)
seqMergeNew (I s:ls) (P m:rs) = _I (take k s) $ seqMergeNew (_I (drop k s) ls) (_P (m-k) rs)
  where k = min m (length s)

--ha beszúrtunk valamit, de töröljük, akkor az mintha nem is lett volna (1 kombináció)
seqMergeNew (I s:ls) (R m:rs) = seqMergeNew (_I (drop k s) ls) (_R (m-k) rs)
  where k = min m (length s)


-------------------------------------------------------------------------------
-- | seq merge 2. változat
-- mindenképpen az inserteket teszi előre


seqMerge2:: [PackedEdit] -> [PackedEdit] -> [PackedEdit]
seqMerge2 x y = flatten $ seqMergeNew2 x y 0

--a törlések számát ideiglensen megőrizzük, hogy az insertek után legyenek

--bármit csináltunk eddig, ha ezután ide beszúrunk, akkor azt szúrjuk be (2 kombináció)
--mert az (R,I) esetet az előzőnél kezeltük 
seqMergeNew2 l (I s:rs) cr = I s: seqMergeNew2 l rs cr

--ha n karaktert eddig töröltünk, akkor az ezután is törölve lesz (3 kombináció)
seqMergeNew2 (R n:ls) r cr = seqMergeNew2 ls r (cr+n)



--ha eddig megőriztünk valamit, és ezután is megőrizzük, akkor azt megőrizzük (1 kombináció)
seqMergeNew2 (P n:ls) (P m:rs) cr = _R cr (P k: seqMergeNew2 (_P (n-k) ls) (_P (m-k) rs) 0)
  where k = min n m
  
--ha valamit eddig megőriztünk, de ezután töröljük, akkor azt végülis töröljük (1 kombináció)
seqMergeNew2 (P n:ls) (R m:rs) cr = _R (k+cr) $ seqMergeNew2 (_P (n-k) ls) (_R (m-k) rs) 0
  where k = min n m
  
--ha beszúrunk valamit, és megőrizzük, akkor azt végülis beszúrtuk (1 kombináció)
seqMergeNew2 (I s:ls) (P m:rs) cr = _I (take k s) $ seqMergeNew2 (_I (drop k s) ls) (_P (m-k) rs) cr
  where k = min m (length s)

--ha beszúrtunk valamit, de töröljük, akkor az mintha nem is lett volna (1 kombináció)
seqMergeNew2 (I s:ls) (R m:rs) cr = seqMergeNew2 (_I (drop k s) ls) (_R (m-k) rs) cr
  where k = min m (length s)

--egyébként elfogyott a jobboldal
seqMergeNew2 l _ cr = _R cr l


--------------------------------------------------------------------------------
{- | A kiegyenlítő összefűzés algoritmusa
--         ----p----(--p'
--         |       |(  |
-- -->r0-->|       |(  r1-->
--         |       |(  |
--         ----q----(--q'
Ha r0 kezdeti revízió után kettéválik a forrás, az egyiken p következik,
a másikon q, akkor p és q párhuzamos összefűzésének eredménye a p' és q'
revíziók, amelyeket rendre p, illetve q után fűzve ugyanahhoz az r1 revízióhoz jutunk.

Tekintsünk 3 résztverőt:
R a tároló, X az egyik, Y a másik kliens.
Azt, hogy a C résztvevő állapota (v,r), azt jelöljük
 C:(v,e)-vel, ahol v a verzió, e az addigi szerkesztésláncok szekvenciális összefésülése.
 
A résztvevő neve után tett kapcsoszárójelek között jelöljük a kliensek
függőben lévő változtatásait.

Elemezzük a következő esetet:
R:(v0,e0)
X:(v0,e0)
Y:(v0,e0)
Tehát (v0,e0) a tároló aktuális állapota, és X és Y is naprakész.
Tegyük fel, hogy időközben más kliens nem tevékenykedik a tárolóban.

1)X és Y is helyi változtatásokat végez
R:(v0,e0)
X:(v0,e0) [x0]
Y:(v0,e0) [y0]

1) X kliens beküldi az x0 revíziót.
R elfogadja x0-t és növeli a verziót. Ezt az információt visszaküldi X-nek.
Ezután a lépés után a következő a helyzet

R:(v1,e0+x0)
X:(v1,e0+x0) []
Y:(v0,e0)    [y0]
2.a) Régi működés: Y kliens beküldi az y0 revíziót
Ezen a ponton program korábbi változata megtagadta y0 letárolását,
és visszaküldte Y-nak a hiányzó x0 revíziót.
Ennek a lépésnek a következménye, hogy mivel x0 megváltoztatta a szöveget,
ezért a következő küldéskor y0 értéke már használhatatlan, azt újra kell kalkulálni..
Tegyük fel, hogy időközben X és Y is újabb változtatásokat végez.
2.a.1) Ekkor az új állapot
R:(v1,e0+x0)
X:(v1,e0+x0) [x1]
Y:(v1,e0+x0) [y1]

2.a.2) X megint megelőzi Y-t, és beküldi az x1 revíziót
A verziók egyeznek, így R elfogadja x1-et, és növeli a verziót, majd jelez X-nek.
R:(v2,e0+x0+x1)
X:(v2,e0+x0+x1) []
Y:(v1,e0+x0) [y1]

2.a.3) Y beküldi az időközben teljesen újraszámolt y1 revízióját
A tároló elutasítja y1-t, mert Y verziója le van maradva, így elküldi Y-nak x1-et.

R:(v2,e0+x0+x1)
X:(v2,e0+x0+x1) []
Y:(v1,e0+x0+x1) [y1]

Ezen a ponton már látszik, hogy Y változtatásai nem kerülnek rögzítésre
a szerveren.
Sok, mondjuk 20 folyamatosan szerkesztő kliens esetén ez oda vezet, hogy 19 rögzítési kérelmet
el kell dobnunk. Ez a hálózati és számítási kapacitás megengedhetetlen pazarlása.
Sok kliens esetén teljesen kiszámíthatatlan, hogy mely kliensek tudnak rögzíteni.

A probléma megoldására az úgynevezett kiegyenlítő összefésülés technikáját alkalmazzuk.

Bonyolítsuk annyival a példát, hogy Y legyen két verzióval lemaradva,
mondjuk mert a felhasználó számítógépe lelassult.
R:(v2,e0+x0+x1)
X:(v2,e0+x0+x1) []
Y:(v0,e0)    [y0]

2.b) Új működés: Y kliens beküldi az y0 revíziót
A tároló megállapítja, hogy Y kliens az (x0+x1) szerkesztéslánccal van lemaradva.
Elkészíti az (x0+x1), illetve az y0 szerkesztésláncok ú.n. kiegyenlítő összefésülését.
Legyen az eredmény a szerkesztésláncokból álló (p0, p1) pár .
A tároló letárolja a p0 láncot új verzióval, visszaküldi Y-nak a p1 szerkesztésláncot.
A résztvevők állapota ezután a következők szerint alakul:
R:(v3,e0+x0+x1+p0)
X:(v2,e0+x0+x1) []
X:(v3,e0+p1) []

A módszer működéséhez természetesen az (x0+x1+p0) == (p1) egyenlőségnek teljesülnie kell.

A kiegyenlítő összefésülés elnevezés arra utal, hogy a művelet eredményei
egyenlővé tesznek két láncot, ha azokat szekvenciális hozzáfűzzük rendre a megfelelő tagokhoz.

-}


parMerge :: ([PackedEdit], [PackedEdit]) -> ([PackedEdit], [PackedEdit])
parMerge (p1, p2) = (\(x,y) -> (flatten x, flatten y)) $ par (p1,p2)

flatten :: [PackedEdit] -> [PackedEdit]
flatten (P n:P m:rest) = flatten (P (n+m):rest)
flatten (R n:R m:rest) = flatten (R (n+m):rest)
flatten (I s1:I s2:rest) = flatten (I (s1++s2):rest)
flatten (l:rest) = l:flatten rest
flatten [] = []

infixl 6 .+.
(.+.) :: ([PackedEdit], [PackedEdit]) -> ([PackedEdit], [PackedEdit]) -> ([PackedEdit], [PackedEdit])
(l1,l2) .+. (r1,r2) = (l1 ++ r1, l2 ++ r2)


par :: ([PackedEdit], [PackedEdit]) -> ([PackedEdit], [PackedEdit])

--ha valamelyik üres, akkor az csináljon meg mindent, amit a másik
par ([], r) = (r, [])
par (r, []) = ([],r)


--ha A beszúrt, akkor szúrjon be B is, A pedig őrizze meg a beszúrását
par (I s1:ls, r) = ([P (length s1)], [I s1]) .+. par (ls, r)
--ha B beszúrt, akkor szúrjon be A is, B pedig őrizze meg a beszúrását
par (l, I s2:rs) = ([I s2], [P (length s2)]) .+. par (l, rs)
--a fenti kettővel definiáltuk, hogy ha mindketten beszúrnak, akkor 
--a bal oldalon beszúrt szöveg megelőzi a jobb oldalit

--ha A és B is hegybenhagyta, akkor a minimumot mindkettő hegybenhaggya
par (P n:ls, P m:rs) = ([P k], [P k]) .+. par (_P (n-k) ls, _P (m-k) rs)
  where k = min m n

--ha A és B is törölt, akkor a minimumot már mindkettő törölte
par (R n:ls, R m:rs) = ([], [])       .+. par (_R (n-k) ls, _R (m-k) rs)
  where k = min m n

--ha A törölt, B pedig nem, akkor azt B-nek is törölnie kell 
par (R n:ls, P m:rs) = ([], [R k])    .+. par (_R (n-k) ls, _P (m-k) rs)
  where k = min m n
par (P n:ls, R m:rs) = ([R k], [])    .+. par (_P (n-k) ls, _R (m-k) rs)
  where k = min m n

--par (xs, ys) = trace ("incompatible edit scripts" ++ (show xs) ++ (show ys) ) ([],[])


-- | Teszteléshez szükséges eszközök

extract :: [PackedEdit] -> String
extract [I str] = str
extract x = error $ "cant extract:" ++ show x


--forrás hossza
oldLength :: [PackedEdit] -> Int
oldLength = foldl (\x y -> case y of
                              I s -> x
                              P n -> x+n
                              R m -> x+m) 0
--cél hossza
newLength :: [PackedEdit] -> Int
newLength = foldl (\x y -> case y of
                              I s -> x+(length s)
                              P n -> x+n
                              R m -> x) 0

--es1-et követheti es2
infixl >.>
es1 >.> es2 = (newLength es1) == (oldLength es2)
                              
instance Arbitrary Edit where
  arbitrary = do
      len <- choose (1, 8) :: Gen Int --length
      n <- choose (0, 2) :: Gen Int 
      case n of
        0 -> do
          str <- vectorOf len $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
          return $ I str
        1 -> do
          return $ P len
        2 -> do
          return $ R len

arbitrarySequence = do    
    lastLen <- choose (0, 4) :: Gen Int --utolsó edit hossza
    first <- liftM flatten $ suchThat arbitrary  ((==lastLen) . newLength) 
    second <- liftM flatten $ suchThat arbitrary  ((==oldLength first) . newLength) 
    third <- liftM flatten $ suchThat arbitrary  ((==oldLength second) . newLength) 
    return (third, second, first)



--a 3 edit script egymás után fűzhető
--ez csak azért kell, hogy biztosan jó adatokat generáljon a teszt
prop_sequentials = forAll arbitrarySequence $ \(es1, es2, es3) -> es1 >.> es2 && es2 >.> es3

--nagyon fontos teszt
--ellenőrzi, hogy a szekvenciális kompozíció asszociatív legyen
prop_associatives =  forAll arbitrarySequence $ \(es1, es2, es3) -> (seqMerge (seqMerge es1 es2) es3) == (seqMerge es1 (seqMerge es2 es3)) 


-- azok a szerkesztésláncok ekvivalensek, amelyekben
--a szomszédos törlések és beszúrások ugyanazok 
equivalent (es1) (es2) = prettify es1 == prettify es2
  where
    prettify es = flatten $ concat $ map (sortBy insertsBeforeRemove) $ groupBy equivalentBlock es
    insertsBeforeRemove (R n) (I s) = GT
    insertsBeforeRemove _ _ = EQ

    equivalentBlock (R _) (I _) = True
    equivalentBlock (I _) (R _) = True
    equivalentBlock (I _) (I _) = True
    equivalentBlock (R _) (R _) = True
    equivalentBlock (P _) (P _) = True
    equivalentBlock _     _     = False
 

arbitraryForParSeq = do    
   
    headLen <- choose (0, 4) :: Gen Int  --első edit hossza
    base <- suchThat arbitrary  ((==headLen) . oldLength) 
    second1 <- suchThat arbitrary  ((==newLength base) . oldLength) 
    second2 <- suchThat arbitrary  ((==newLength base) . oldLength) 
    return (flatten base, ( flatten second1, flatten second2))
    

prop_parsequ1 = forAll arbitraryForParSeq $ \(b,(s1,s2)) -> let (p1, p2) = parMerge (s1, s2) in
      (seqMerge (seqMerge b s1) p1) `equivalent` (seqMerge (seqMerge b s2) p2)



prop_parsequ2 = forAll arbitraryForParSeq $ \(b,(s1,s2)) -> let (p1, p2) = parMerge (s1, s2) in
      (seqMerge2 (seqMerge2 b s1) p1) == (seqMerge2 (seqMerge2 b s2) p2)

runTests = $quickCheckAll

