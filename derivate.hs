main :: IO()
main  = return ()

data Const = Num Float | Con Char
         deriving Show
data Fun = Val Const | Var Char | Log Const Fun | Ln Fun
          | Pow Fun Const | Exp Const Fun | Sc Const Fun
          | Div Fun Fun | Mul Fun Fun | Plus Fun Fun | Minus Fun Fun
         deriving Show




derive :: Fun -> Maybe Fun
-- de baza
derive (Val _) = return (Val (Num 0))
derive (Var _) = return (Val (Num 1))
derive (Sc c f) = do f' <- derive f
                     return (Mul (Val c) f')
derive (Pow f (Num n)) = do f' <- derive f
                            return (Mul f' (Mul (Val (Num n)) (Pow f (Num (n-1)))))
derive (Exp (Con 'e') f) = do f' <- derive f
                              return (Mul f' (Exp (Con 'e') f))
derive (Exp x f) = do f' <- derive f
                      return (Mul (Mul f' (Exp x f)) (Ln (Val x)))
derive (Ln f) = do f' <- derive f
                   return (Mul f' (Div (Val (Num 1)) f))
derive (Log (Con 'e') f) = do f' <- derive f
                              return (Mul f' (Div (Val (Num 1)) f))
derive (Log b f) = do f' <- derive f
                      return (Mul f' (Div (Val (Num 1)) (Mul f (Ln (Val b)))))

-- adunari, scaderi, inmultiri, impartiri
derive (Plus f g) = do g' <- derive g
                       f' <- derive f
                       return (Plus f' g')
derive (Minus f g) = do g' <- derive g
                        f' <- derive f
                        return (Minus f' g')
derive (Mul f g) = do g' <- derive g
                      f' <- derive f
                      return (Plus (Mul f' g) (Mul f g'))
derive (Div f g) = do g' <- derive g
                      f' <- derive f
                      return (Div (Minus (Mul f' g) (Mul f g')) (Pow g (Num 2)))

-- and we deal with nothing else
derive _ = Nothing

number :: Fun -> Maybe Float
number (Val (Num x)) = return x
number _ = Nothing

--reduce :: Fun -> Maybe Fun
--reduce (Plus a b) | number a == Just x = 
