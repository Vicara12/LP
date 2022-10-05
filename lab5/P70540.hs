import Control.Monad

data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr


-- 1)

eval1 :: Expr -> Int
eval1 (Val i) = i
eval1 (Add e1 e2) = (eval1 e1) + (eval1 e2)
eval1 (Sub e1 e2) = (eval1 e1) - (eval1 e2)
eval1 (Mul e1 e2) = (eval1 e1) * (eval1 e2)
eval1 (Div e1 e2) = div (eval1 e1) (eval1 e2)

-- 2)


eval2 :: Expr -> Maybe Int
eval2 (Val i) = Just i
eval2 (Add e1 e2) = do
    v1 <- (eval2 e1)
    v2 <- (eval2 e2)
    return $ v1 + v2
eval2 (Sub e1 e2) = liftM2 (-) (eval2 e1) (eval2 e2)
eval2 (Mul e1 e2) = liftM2 (*) (eval2 e1) (eval2 e2)
eval2 (Div e1 e2) = do
    ve1 <- (eval2 e1)
    ve2 <- (eval2 e2)
    if ve2 == 0 then Nothing else return $ div ve1 ve2

-- 3)

eval3 :: Expr -> Either String Int
eval3 (Val i) = Right i
eval3 (Add e1 e2) = do
    ve1 <- (eval3 e1)
    ve2 <- (eval3 e2)
    return $ ve1 + ve2
eval3 (Sub e1 e2) = do
    ve1 <- (eval3 e1)
    ve2 <- (eval3 e2)
    return $ ve1 - ve2
eval3 (Mul e1 e2) = do
    ve1 <- (eval3 e1)
    ve2 <- (eval3 e2)
    return $ ve1 * ve2
eval3 (Div e1 e2) = do
    ve1 <- (eval3 e1)
    ve2 <- (eval3 e2)
    if ve2 == 0 then (Left "div0") else return $ div ve1 ve2
