module RefList (sList) where

import Language.Haskell.TH

sList :: [String] -> Q [Dec]
sList ss = return $ [ValD
    (VarP $ mkName "slns")
    (NormalB $ ListE $ do
        s <- ss
        return $ VarE $ mkName $ s ++ ".Main.main")
    []]