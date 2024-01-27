module Lambda where

import Expr
import Data.List

-- find free variables of a Expr
free_vars :: Expr -> [String]
free_vars (Variable x) = [x]
free_vars (Function x e) = filter (/= x) (free_vars e)
free_vars (Application e1 e2) = nub $ free_vars e1 ++ free_vars e2

-- reduce a redex
reduce :: Expr -> String -> Expr -> Expr
reduce (Variable y) x e2
    | y == x = e2
    | otherwise = Variable y
reduce (Function y e) x e2
    | y == x = Function x e
    | y `notElem` free_vars e2 = Function y (reduce e x e2)
    | otherwise = reduce newBody x e2
    where
        z = head (filter (\x -> x `notElem` free_vars e && x `notElem` free_vars e2) (map (("x" ++) . show) [1..]))
        newBody = Function z (reduce e y (Variable z))
reduce (Application e1 e2) x e3 =
    Application (reduce e1 x e3) (reduce e2 x e3)

-- Auxiliary function that checks if an Expr contains a redex
containsRedex :: Expr -> Bool
containsRedex (Variable x) = False
containsRedex (Function x e) = containsRedex e
containsRedex (Application (Function x e1) e2) = True
containsRedex (Application e1 e2) = containsRedex e1 || containsRedex e2

-- Normal Evaluation
-- perform one step of Normal Evaluation
stepN :: Expr -> Expr
stepN (Variable x) = Variable x
stepN (Function x e) = Function x (stepN e)
stepN (Application (Function x e1) e2) = reduce e1 x e2
stepN (Application e1 e2) = if containsRedex e1 then Application (stepN e1) e2 else Application e1 (stepN e2)

-- perform Normal Evaluation
reduceN :: Expr -> Expr
reduceN e = if containsRedex e then reduceN (stepN e) else e

reduceAllN :: Expr -> [Expr]
reduceAllN e = if containsRedex e then e : reduceAllN (stepN e) else [e]

-- Applicative Evaluation
-- perform one step of Applicative Evaluation
stepA :: Expr -> Expr
stepA (Variable x) = Variable x
stepA (Function x e) = Function x (stepA e)
stepA (Application (Function x e1) e2) = if containsRedex e2 then Application (Function x e1) (stepA e2) else reduce e1 x e2
stepA (Application e1 e2) = if containsRedex e1 then Application (stepA e1) e2 else Application e1 (stepA e2)

-- perform Applicative Evaluation
reduceA :: Expr -> Expr
reduceA e = if containsRedex e then reduceA (stepA e) else e

reduceAllA :: Expr -> [Expr]
reduceAllA e = if containsRedex e then e : reduceAllA (stepA e) else [e]

-- make substitutions into a expression with Macros
evalMacros :: [(String, Expr)] -> Expr -> Expr
evalMacros computationalContext (Variable x) = Variable x
evalMacros computationalContext (Function x e) = Function x (evalMacros computationalContext e)
evalMacros computationalContext (Application e1 e2) = Application (evalMacros computationalContext e1) (evalMacros computationalContext e2)
evalMacros computationalContext (Macro m) =
    case lookup m computationalContext of
        Just e -> evalMacros computationalContext e
        Nothing -> Macro m

-- evaluate code sequence using given strategy
evalCode :: (Expr -> Expr) -> [Code] -> [Expr]
evalCode strategy codeLines = evalCodeHelper strategy codeLines []
    where
        evalCodeHelper :: (Expr -> Expr) -> [Code] -> [(String, Expr)] -> [Expr]
        evalCodeHelper strategy [] _ = []
        evalCodeHelper strategy (x : xs) computationalContext = 
            case x of
                Evaluate e -> strategy (evalMacros computationalContext e) : evalCodeHelper strategy xs computationalContext
                Assign m e -> evalCodeHelper strategy xs (updateComputationalContext m e computationalContext)

        updateComputationalContext :: String -> Expr -> [(String, Expr)] -> [(String, Expr)]
        updateComputationalContext newMacro newExpr [] = [(newMacro, newExpr)]
        updateComputationalContext newMacro newExpr ((existingMacro, existingExpr) : currentComputationalContext)
            | newMacro == existingMacro = (existingMacro, newExpr) : currentComputationalContext
            | otherwise = (existingMacro, existingExpr) : updateComputationalContext newMacro newExpr currentComputationalContext
