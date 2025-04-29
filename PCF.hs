type N = Integer
type B = Bool

split :: (r,s) -> (r -> s -> t) -> t
split (s,t) f = f s t

tt = True
ff = False

s = (+1)
p = subtract 1
z = (==0)

cases :: B -> t -> t -> t
cases True  s _ = s
cases False _ t = t

y :: (t -> t) -> t
y t = t (y t)

----------------------------------------

plus :: N -> N -> N
-- plus m n = cases (z m) n (s (plus (p m) n))
-- gammaplus f m n = cases (z m) n (s (f (p m) n))
-- plus = y gammaplus
plus = y (\f m n -> cases (z m) n (s (f (p m) n)))

mult :: N -> N -> N
mult = y (\f m n -> cases (z m) 0 (plus n (f (p m) n)))

fact :: N -> N
fact = y (\f n -> cases (z n) (s 0) (mult n (f (p n))))

isEven :: N -> B
isEven = y (\f n -> cases (z n) tt (cases (f (p n)) ff tt))
