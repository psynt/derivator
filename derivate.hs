main :: IO()
main  = return ()

--data Const = Num Float | Con Char
--         deriving Show
data Oper = Div | Mul | Minus | Plus
          deriving Show

data Fun a = Num a | Const Char | Var Char
          | Log (Fun a) (Fun a) | Ln (Fun a)
          | Exp (Fun a) (Fun a)
          | O Oper (Fun a) (Fun a)
         deriving Show

-- instance Functor Fun where
--   --fmap :: (a->b) -> Fun a-> Fun b
--   fmap f (Num x) = Num (f x)
--   fmap _ (Const c) = Const c
--   fmap _ (Var x) = Var x
--   fmap f (Ln g) = Ln (fmap f g)
--   fmap f (Log b n) = Log (fmap f b) (fmap f n)
--   fmap f (Exp b n) = Exp (fmap f b) (fmap f n)
--   fmap f (O x g h) = O x (fmap f g) (fmap f h)
--
-- instance Applicative Fun where
--   -- pure :: a -> Fun a
--   pure = Num
--   -- <*> :: Fun (a -> b) -> Fun a -> Fun b
--   Num f <*> x = fmap f x
--   -- _ <*> (Const c) = Const c
--   -- _ <*> (Var x) = Var x
--   -- f <*> (Ln g) = Ln (f <*> g)
--   -- f <*> (Log b n) = Log (f <*> b) (f <*> n)
--   -- f <*> (Exp b n) = Exp (f <*> b) (f <*> n)
--   -- f <*> (O x g h) = O x (f <*> g) (f <*> h)
--
-- instance Monad Fun where
--   -- return :: a -> Fun a
--   -- (>>=) :: Fun a -> (a -> Fun b) -> Fun b
--   Num n >>= f = f n
--   Const c >>= _ = Const c
--   Var x >>=


derive :: Fun Float -> Maybe (Fun Float)
-- de baza
derive (Num _) = return (Num 0)
derive (Const _) = return (Num 0)
derive (Var _) = return (Num 1)
derive (O Mul (Num n) f) = do f' <- derive f
                              return (O Mul (Num n) f')
derive (O Mul (Const c) f) = do f' <- derive f
                                return (O Mul (Const c) f')
derive (Exp f (Num n)) = do f' <- derive f                                        -- is really just Pow
                            return (O Mul f' (O Mul (Num n) (Exp f (Num (n-1)))))
derive (Exp (Const 'e') f) = do f' <- derive f
                                return (O Mul f' (Exp (Const 'e') f))
derive (Exp x f) = do f' <- derive f
                      return (O Mul (O Mul f' (Exp x f)) (Ln x))
derive (Ln f) = do f' <- derive f
                   return (O Mul f' (O Div (Num 1) f))
derive (Log (Const 'e') f) = do f' <- derive f
                                return (O Mul f' (O Div (Num 1) f))
derive (Log b f) = do f' <- derive f
                      return (O Mul f' (O Div (Num 1) (O Mul f (Ln b))))

-- adunari, scaderi, inmultiri, impartiri
derive (O Mul f g) = do g' <- derive g
                        f' <- derive f
                        return (O Plus (O Mul f' g) (O Mul f g'))
derive (O Div f g) = do g' <- derive g
                        f' <- derive f
                        return (O Div (O Minus (O Mul f' g) (O Mul f g')) (Exp g (Num 2)))
derive (O x f g) = do g' <- derive g
                      f' <- derive f
                      return (O x f' g')

-- and we deal with nothing else
derive _ = Nothing
