main :: IO()
main  = return ()

data Oper = Div | Mul | Minus | Plus | Log | Exp
          deriving (Show, Eq)

data Fun a = Num a | Const Char | Var Char
          | O Oper (Fun a) (Fun a)
         deriving (Show, Eq)

-- instance Functor Fun where
--   --fmap :: (a->b) -> Fun a-> Fun b
--   fmap f (Num x) = Num (f x)
--   fmap _ (Const c) = Const c
--   fmap _ (Var x) = Var x
--   fmap f (O x g h) = O x (fmap f g) (fmap f h)
--
-- instance Applicative Fun where
--   -- pure :: a -> Fun a
--   pure = Num
--   -- <*> :: Fun (a -> b) -> Fun a -> Fun b
--   Num f <*> x = fmap f x
--   -- _ <*> (Const c) = Const c
--   -- _ <*> (Var x) = Var x
--   -- f <*> (O x g h) = O x (f <*> g) (f <*> h)
--
-- instance Monad Fun where
--   -- return :: a -> Fun a
--   -- (>>=) :: Fun a -> (a -> Fun b) -> Fun b
--   Num n >>= f = f n
--   Const c >>= _ = Const c
--   -- Var x >>=


derive :: Fun Float -> Maybe (Fun Float)
-- de baza
derive (Num _) = return (Num 0)
derive (Const _) = return (Num 0)
derive (Var _) = return (Num 1)
derive (O Mul (Num n) f) = do f' <- derive f
                              shorten (O Mul (Num n) f')
derive (O Mul (Const c) f) = do f' <- derive f
                                shorten (O Mul (Const c) f')
derive (O Exp f (Num n)) = do f' <- derive f             -- is really just Pow
                              ex <- shorten (O Exp f (Num (n-1)))
                              m <- shorten (O Mul (Num n) ex)
                              shorten (O Mul f' m)
derive (O Exp (Const 'e') f) = do f' <- derive f
                                  ex <- shorten (O Exp (Const 'e') f)
                                  shorten (O Mul f' ex)
derive (O Exp x f) = do f' <- derive f
                        lg <- shorten (O Log (Const 'e') x)
                        ex <- shorten (O Exp x f)
                        m <- shorten (O Mul f' ex)
                        shorten (O Mul m lg)
derive (O Log (Const 'e') f) = do f' <- derive f
                                  shorten (O Mul f' (O Div (Num 1) f))
derive (O Log b f) = do f' <- derive f
                        lg <- shorten (O Log (Const 'e') b)
                        m <- shorten (O Mul f lg)
                        shorten (O Mul f' (O Div (Num 1) m))

-- adunari, scaderi, inmultiri, impartiri
derive (O Mul f g) = do g' <- derive g
                        f' <- derive f
                        return (O Plus (O Mul f' g) (O Mul f g'))
derive (O Div f g) = do g' <- derive g
                        f' <- derive f
                        return (O Div (O Minus (O Mul f' g) (O Mul f g')) (O Exp g (Num 2)))
derive (O x f g) = do g' <- derive g
                      f' <- derive f
                      return (O x f' g')


-- and we deal with nothing else
-- derive _ = Nothing

shorten :: (Monad m, Num a, Eq a) => Fun a -> m (Fun a)
shorten (O Mul (Num x) (Num y)) = return (Num (x*y))
shorten (O Mul (Num 1) x) = return x
shorten (O Mul x (Num 1)) = return x
shorten (O Mul x y) = return (O Mul x y)
shorten (O Exp _ (Num 0)) = return (Num 1)
shorten (O Exp (Num 1) _) = return (Num 1)
shorten (O Exp x (Num 1)) = return x
shorten (O Exp x y) = return (O Mul x y)
shorten (O Log x y) | x == y = return (Num 1)
                    | otherwise = return (O Log x y)
