{-# LANGUAGE
    TemplateHaskell,
    LambdaCase
    #-}

module ConstraintWitness.TH.Generate where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

generateDictionaries :: Name -> Q [Dec]
generateDictionaries clsname = do
    reify clsname >>= \case
        ClassI (ClassD super _ tvars _ meths) instances ->
            return (classDictD : (instances >>= \case
                InstanceD cxt ihead _ ->
                    let iname = mkName $ "inst" ++ (nameBase clsname) ++ "'" ++ flatten cxt ++ "'" ++ flatten ihead
                    in [ValD (VarP iname) (NormalB $ RecConE classDictName [(mkName ("_" ++ nameBase methname), VarE methname) | (SigD methname _) <- meths]) []]))
              where
                classDictName = mkName (nameBase clsname ++ "Dict")
                classDictD = DataD [] classDictName tvars [RecC classDictName [(mkName ("_" ++ nameBase methname), NotStrict, methtype) | (SigD methname methtype) <- meths]] []
        _ -> return []

-- | Collapses pretty-printable thing into a valid identifier
flatten :: Ppr a => a -> String
flatten x = pprint x >>= \case
    ' ' -> "__"
    '.' -> "_i"
    ':' -> "_l"
    '+' -> "_p"
    '(' -> "_o"
    ')' -> "_c"
    '[' -> "List_o"
    ']' -> "_c"
    c   -> [c]