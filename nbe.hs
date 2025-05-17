import Data.Unique
import Data.Maybe

data T = TVar String | T :=> T
  deriving (Eq, Show, Read, Ord)

ti = TVar "alpha" :=> TVar "alpha"

tc = ti :=> ti

data L = Var String | L :@ L | Abs String L
  deriving (Eq, Show, Read, Ord)

data H = Base { var :: L } | Fun { fun :: H -> IO H }

-- fun (Fun f) = f

i = Abs "x" $ Var "x"

k = Abs "x" $ Abs "y" $ Var "x"

s = Abs "x" $ Abs "y" $ Abs "z" $ (Var "x" :@ Var "z") :@ (Var "y" :@ Var "z")

c3 = Abs "f" $ Abs "x" $ Var "f" :@ (Var "f" :@ (Var "f" :@ Var "x"))

c n = Abs "f" $ Abs "x" $ iterate (Var "f" :@) (Var "x") !! n

cplus = Abs "m" $ Abs "n" $ Abs "f" $ Abs "x" ((Var "m" :@ Var "f") :@ ((Var "n" :@  Var "f") :@ Var "x"))

genSym :: String -> IO String
genSym name = (name++) . show . hashUnique <$> newUnique

type Valuation = String -> IO H

modify :: Valuation -> String -> H -> Valuation
modify xi x a y
 | x == y = return a
 | x /= y = xi y

evaluate :: L -> Valuation -> IO H
evaluate (Var x)    xi = xi x
evaluate (m1 :@ m2) xi = do
  a <- evaluate m2 xi
  f <- evaluate m1 xi
  fun f a
evaluate (Abs x n)  xi = return $ Fun $ \a -> evaluate n $ modify xi x a

reflect :: T -> L -> IO H
reflect (TVar _) m = return $ Base m
reflect (r :=> s) m = return $ Fun (\a -> do n <- reify r a
                                             reflect s (m :@ n))

reify :: T -> H -> IO L
reify (TVar _) (Base m) = return m
reify (r :=> s) (Fun f) =
  do
    x <- genSym "x"
    a <- reflect r (Var x)
    b <- f a
    c <- reify s b
    return $ Abs x c

type Context = [(String, T)]

nbe :: Context -> T -> L -> IO L
nbe g t m = do a <- evaluate m (\x -> reflect (fromJust (lookup x g)) (Var x))
               reify t a

skk = nbe [] ti $ s :@ k :@k
c8  = nbe [] tc $ cplus :@ (c 3) :@ (c 5)
fffx = nbe [("f", ti), ("x", TVar "alpha")] (TVar "alpha") $ (c 3) :@ Var "f" :@ Var "x"
