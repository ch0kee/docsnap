module TestDebug

data Expr a = I Int
            | B Bool
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Eq  (Expr a) (Expr a)

add :: Expr Int -> Expr Int -> Expr Int
add = Add

i :: Int  -> Expr Int
i = I
b :: Bool -> Expr Bool
b = B

eval :: Expr a -> a
eval (I 5 :: Expr Int)


pure (.)

pure :: a -> f a

(.) :: (b -> c) -> (a -> b) -> a -> c
