import Data.Unique

data T = TVar String | T :=> T
  deriving (Eq, Show, Read, Ord)

ti = TVar "alpha" :=> TVar "alpha"
tc = ti :=> ti

data L = Var String | L :@ L | Abs String L
  deriving (Eq, Show, Read, Ord)

data H = Base { var :: L } | Fun { fun :: H -> H }

-- fun (Fun f) = f

i = Abs "x" $ Var "x"

k = Abs "x" $ Abs "y" $ Var "x"

s = Abs "x" $ Abs "y" $ Abs "z" $ (Var "x" :@ Var "z") :@ (Var "y" :@ Var "z")

c3 = Abs "f" $ Abs "x" $ Var "f" :@ (Var "f" :@ (Var "f" :@ Var "x"))

c n = Abs "f" $ Abs "x" $ iterate (Var "f" :@) (Var "x") !! n

cplus = Abs "m" $ Abs "n" $ Abs "f" $ Abs "x" ((Var "m" :@ Var "f") :@ ((Var "n" :@  Var "f") :@ Var "x"))

genSym :: String -> IO String
genSym name = (name++) . show . hashUnique <$> newUnique

type Valuation = String -> H

modify :: Valuation -> String -> H -> Valuation
modify xi x a y
 | x == y = a
 | x /= y = xi y

evaluate :: L -> Valuation -> H
evaluate (Var x)    xi = xi x
evaluate (m1 :@ m2) xi = fun (evaluate m1 xi) $ evaluate m2 xi
evaluate (Abs x n)  xi = Fun $ \a -> evaluate n $ modify xi x a

reflect :: T -> L -> H
reflect (TVar _) m = Base m
reflect (r :=> s) m = Fun (\a -> reflect s (m :@ reify r a))

reify :: T -> H -> L
reify (TVar _) (Base m) = m
reify (r :=> s) (Fun f) = Abs x (reify s (f (reflect r (Var x))))
  where x = "x"

nbe :: T -> L -> L
nbe t m = reify t (evaluate m error)

