{-#LANGUAGE TemplateHaskell#-}
module ShowTH where

import Language.Haskell.TH
import Control.Monad (replicateM)
import Data.List (intercalate)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

formatNormal :: Name -> [Name] -> Q Exp
formatNormal con xs = [| con' <> " " <> unwords $(listE fields) |] where
  con' = nameBase con
  fields = (\x -> [| show $(varE x) |]) <$> xs

formatField :: Name -> Name -> Q Exp
formatField x r = [| r' <> " = " <> show $(varE x) |] where
  r' = nameBase r

formatRec :: Name -> [(Name, Name)] -> Q Exp
formatRec con xrs = [| con' <> " { " <> $(fields) <> " }" |] where
  con' = nameBase con
  fields = [| intercalate ", " $(listE $ uncurry formatField <$> xrs) |]

newXs :: Int -> Q [Name]
newXs n = replicateM n $ newName "x"

derivingShow :: Name -> Q [Dec]
derivingShow name = do
  TyConI (DataD _ _ _ _ cons' _) <- reify name
  showD <- showF cons'
  return [InstanceD Nothing [] (showT name) [showD]]
  where
    showT :: Name -> Type
    showT s = AppT (ConT ''Show) (ConT s)
    showF :: [Con] -> Q Dec
    showF cons' = FunD 'show <$> mapM clause cons'
    clause :: Con -> Q Clause
    clause (NormalC n ts) = do
      xs <- newXs $ length ts
      f  <- formatNormal n xs
      return $ Clause [ConP n $ VarP <$> xs] (NormalB f) []
    clause (RecC    n ts) = do
      xs <- newXs $ length ts
      f <- formatRec n $ zip xs (fst3 <$> ts)
      return $ Clause [ConP n $ VarP <$> xs] (NormalB f) []
    clause _              = fail "unspported data type"
