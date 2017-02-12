{-# LANGUAGE TemplateHaskell, NoImplicitPrelude, MultiParamTypeClasses #-}

module Magic (magicNat, magicList) where

import qualified Prelude as P
import qualified Data.List as L
import qualified GHC.Exts as G.E
import Language.Haskell.TH

conSize :: Con -> P.Int
conSize (NormalC _ l) = P.length l
conSize (RecC _ l) = P.length l
conSize (InfixC _ _ _) = 2
conSize (ForallC _ _ c) = conSize c
conSize (GadtC _ l _) = P.length l
conSize (RecGadtC _ l _) = P.length l

magicNat :: Name -> DecsQ
magicNat natName = do
  TyConI (DataD _ _ _ _ natCons _) <- reify natName
  let [NormalC zeroName [], NormalC succName [_]] = L.sortOn conSize natCons
  [d|
    instance P.Num $(conT natName) where
      fromInteger x = case x of
        0 -> $(conE zeroName)
        n -> $(conE succName) (P.fromInteger (n P.- 1))
      (+) = P.error "Use your own addition!"
      (*) = P.error "Use your own multiplication!"
      (-) = P.error "Use your own subtraction!"
      negate = P.error "Negation not possible for Nat"
      abs = P.id
      signum _ = 1

    instance P.Show $(conT natName) where
      show = P.show P.. natToInteger where
        natToInteger :: $(conT natName) -> P.Integer
        natToInteger x = $(do
          aName <- newName "a"
          [e|
            case x of
              $(conP zeroName [])           -> 0
              $(conP succName [varP aName]) -> natToInteger $(varE aName) P.+ 1
            |])
    |]

tyVarToType :: TyVarBndr -> TypeQ
tyVarToType (PlainTV name) = varT name
tyVarToType (KindedTV name _) = varT name

class NC
instance NC

magicList :: Name -> DecsQ
magicList listName = do
  TyConI (DataD _ _ tyVars _ listCons _) <- reify listName
  let
    [NormalC nilName [], NormalC consName [(_, elemType), _]] = L.sortOn conSize listCons
    tyParams = L.map tyVarToType tyVars
    instHead = P.foldl appT (conT listName) tyParams
    showCtx = case tyVars of
      [] -> [t| NC |]
      _  -> [t| P.Show $(P.pure elemType) |]
  [d|
    instance $(showCtx) => P.Show $(instHead) where
      show = P.show P.. G.E.toList

    instance G.E.IsList $(instHead) where
      type Item $(instHead) = $(P.pure elemType)
      fromList = P.foldr $(conE consName) $(conE nilName)
      toList l = $(do
        aName <- newName "a"
        xsName <- newName "xs"
        [e|
          case l of
            $(conP nilName []) -> []
            $(conP consName [varP aName, varP xsName]) ->
              $(varE aName) : G.E.toList $(varE xsName)
          |])
    |]
