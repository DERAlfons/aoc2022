module Day21.Main (main) where

import Data.Map (Map, fromList, insert, (!))
import Data.Maybe (maybeToList)
import Data.Foldable (asum)

import My.Parser (Parser, parserRegex, run)

data Expr = Value Int | Var String | Op String String String
data Term = Term Rational Rational

evalEq :: Map String Expr -> String -> Term
evalEq m e = case m ! e of
    Value v -> Term 0 (fromIntegral v)
    Var _ -> Term 1 0
    Op op r1 r2 -> let
        Term v1 c1 = evalEq m r1
        Term v2 c2 = evalEq m r2 in
        case op of
            "+" -> Term (v1 + v2) (c1 + c2)
            "-" -> Term (v1 - v2) (c1 - c2)
            "*" -> Term (v1 * c2 + v2 * c1) (c1 * c2)
            "/" -> Term (v1 / c2) (c1 / c2)
            "=" -> Term (v1 - v2) (c1 - c2)

parseE :: String -> Maybe (String, Expr)
parseE = run $ asum [
    parserRegex "(\\w+): (\\d+)" $ \[id, v] -> (id, Value (read v)),
    parserRegex "(\\w+): (\\w+) (.) (\\w+)" $ \[id, r1, op, r2] -> (id, Op op r1 r2)]

main :: IO (String, String)
main = do
    m <- fromList <$> (maybeToList . parseE =<<) <$>
        lines <$> readFile "Day21/input.txt"

    let root = (\(Term _ c) -> round c) $ evalEq m "root"
    putStr "Value of root monkey: "
    print root

    let Op _ r1 r2 = m ! "root"
        m2 = insert "humn" (Var "x") $ insert "root" (Op "=" r1 r2) m
        humn = (\(Term v c) -> round $ c / (-v)) $ evalEq m2 "root"
    putStr "Value of humn that solves root equation: "
    print humn

    return (show root, show humn)