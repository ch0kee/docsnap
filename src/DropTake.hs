    concatES :: [Edit] -> [Edit] -> [Edit]
    concatES (Preserve:ls) (Preserve:rs) = Preserve:concatES ls rs
merge (x:xs) (y:ys) = x: merge xs ys

    concatES (Preserve:ls) (Remove:rs) = (Remove:concatES ls rs)
merge (x:xs) (y:ys) = y: merge xs ys

    concatES l@(Preserve:ls) (Insert c:rs) = (Insert c:concatES l rs)
merge (x:xs) (y:ys) = y: merge (x:xs) ys

    concatES (Remove:ls) r = (Remove:concatES ls r)
merge (x:xs) (y:ys) = x: merge xs (y:ys)

    concatES (Insert c:ls) (Preserve:rs) = (Insert c:concatES ls rs)
merge (x:xs) (y:ys) = x: merge xs ys
    concatES (Insert c:ls) (Remove:rs) = concatES ls rs
merge (x:xs) (y:ys) = merge xs ys

    concatES l@(Insert _:ls) (Insert c:rs) = (Insert c:concatES l rs)
merge (x:xs) (y:ys) = y: merge (x:xs) ys

    concatES [] r = r
merge [] rest = rest



mergeSingle Preserve Preserve = \xs ys -> l

merge (x:xs) (y:ys) = x: merge xs ys

([SingleEdit] -> [SingleEdit]) -> SingleEdit

(take,drop)
(drop,take)
(back,take)

data MergeAction = Drop | Back | Take

action Back (x:xs) = x:xs
action Drop (x:xs) = xs 
action Take (x:xs) = xs 

take'back
take'drop
drop'drop
back'take

dropSnd
dropFst
take

