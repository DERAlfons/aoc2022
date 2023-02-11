module GenMains (genMains) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Regex.PCRE ((=~))

getModuleNames :: ModuleInfo -> [String]
getModuleNames (ModuleInfo mList) = do
    Module _ (ModName name) <- mList
    return name

genMains :: Q Exp
genMains = do
    moduleNames <- getModuleNames <$> (reifyModule =<< thisModule)
    return $ ListE $ do
        moduleName <- filter (=~ "^Day\\d+\\.Main$") moduleNames
        let day = LitE $ StringL $ takeWhile (/= '.') moduleName
            main = VarE $ mkName $ moduleName ++ ".main"
        return $ TupE [Just day, Just main]