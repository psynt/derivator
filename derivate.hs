main :: IO()
main  = return ()

--data Const = Num Float | Con Char
--         deriving Show
data Oper = Div | Mul | Minus | Plus | Log | Exp
          deriving Show

data Fun a = Num a | Const Char | Var Char
          | O Oper (Fun a) (Fun a)
         deriving Show

instance Functor Fun where
  --fmap :: (a->b) -> Fun a-> Fun b
  fmap f (Num x) = Num (f x)
  fmap _ (Const c) = Const c
  fmap _ (Var x) = Var x
  fmap f (O x g h) = O x (fmap f g) (fmap f h)

instance Applicative Fun where
  -- pure :: a -> Fun a
  pure = Num
  -- <*> :: Fun (a -> b) -> Fun a -> Fun b
  Num f <*> x = fmap f x
  -- _ <*> (Const c) = Const c
  -- _ <*> (Var x) = Var x
  -- f <*> (Ln g) = Ln (f <*> g)
  -- f <*> (Log b n) = Log (f <*> b) (f <*> n)
  -- f <*> (Exp b n) = Exp (f <*> b) (f <*> n)
  -- f <*> (O x g h) = O x (f <*> g) (f <*> h)

instance Monad Fun where
  -- return :: a -> Fun a
  -- (>>=) :: Fun a -> (a -> Fun b) -> Fun b
  Num n >>= f = f n
  Const c >>= _ = Const c
  -- Var x >>=


derive :: Fun Float -> Maybe (Fun Float)
-- de baza
derive (Num _) = return (Num 0)
derive (Const _) = return (Num 0)
derive (Var _) = return (Num 1)
derive (O Mul (Num n) f) = do f' <- derive f
                              return (O Mul (Num n) f')
derive (O Mul (Const c) f) = do f' <- derive f
                                return (O Mul (Const c) f')
derive (O Exp f (Num n)) = do f' <- derive f             -- is really just Pow
                              return (O Mul f' (O Mul (Num n) (O Exp f (Num (n-1)))))
derive (O Exp (Const 'e') f) = do f' <- derive f
                                  return (O Mul f' (O Exp (Const 'e') f))
derive (O Exp x f) = do f' <- derive f
                        return (O Mul (O Mul f' (O Exp x f)) (O Log (Const 'e') x))
derive (O Log (Const 'e') f) = do f' <- derive f
                                  return (O Mul f' (O Div (Num 1) f))
derive (O Log b f) = do f' <- derive f
                        return (O Mul f' (O Div (Num 1) (O Mul f (O Log (Const 'e') b))))

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
shorten (O Mul (Num n) (Var c)) = return (O Mul (Num n) (Var c))
shorten (O Mul (Num n) (Const c)) = return (O Mul (Num n) (Const c))
shorten (O Mul (Num n) x) = return (fmap (n*) x)
shorten (O Mul x (Num n)) = return (fmap (n*) x)
shorten (O Mul x y) = return (O Mul x y)
-- shorten (O Log (Const 'e') (Const 'e')) = return 1


-- operator Plus = return (+)
-- operator Minus = return (-)
-- operator Mul = return (*)
-- operator Div = return (/)
--
-- operate :: Fun Float -> Fun Float
-- operate (O x f g) = do rf <- reduce f
--                        rg <- reduce g
--                        o <- operator x
--                        return (rf `o` rg)
--
--
-- reduce (Num a) = return (Num a)
-- reduce (Const c) = return (Const c)
-- reduce (Var c) = return (Var c)
